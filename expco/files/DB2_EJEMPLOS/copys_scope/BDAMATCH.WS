REMARK*BDAMATCH-WS.v.0003.
REMARK********************************************************************
REMARK* COPY QUE CONTIENE LA VARIABLES NECESARIAS PARA ELECCION DE MEJOR 
REMARK* PORTAFOLI Y MATCH DE NOMBRES A PARTIR DE LA INFORMACION DE LOS      
REMARK* ARCHIVO BDACSV, ICBDIR ICMCRE E ICMCHK.
REMARK********************************************************************
REMARK* RMB0002 *VIGE01: SE CONTRARAN LAS CUENTAS CON ADEJETIVOS DE 
REMARK*         *MUERTOS Y EL NUMERO TOTAL DE CUENTAS DEL MEJOR PORTAFOLIO     
REMARK********************************************************************
REMARK* RMB0003     * PESO01 - OPTIMIZAR MANEJO DEL CALCULO DEL PESO     *
REMARK* R-MARTINEZ. *          DEL PORTAFOLIO .                          *
REMARK* 5-SEP-2011  * FUEN01 - NCLUIR  FUENTES EXTERNAS                  *
REMARK*             * APEL01 - INCLUIR PRIMER APELLIDO EN MATCH          *
REMARK*             * NOMB01 - MAQUILLAR EL MEJOR NOMBRE EN PRESENTACION *
REMARK********************************************************************
NOMB01 01 VARIABLES-MAQ.                                                RMB0003
NOMB01     03 I-MAQ                    PIC 9(03)  VALUE 0.              RMB0003
NOMB01     03 J-MAQ                    PIC 9(03)  VALUE 0.              RMB0003
NOMB01     03 SW-BORRAR-MAQ            PIC 9(03)  VALUE 0.              RMB0003
NOMB01     03 NOMBRE-INPMAQ-ORI        PIC X(45)  VALUE SPACES.         RMB0003
NOMB01     03 NOMBRE-OUTMAQ-OK         PIC X(45)  VALUE SPACES.         RMB0003
NOMB01     03 NOMBRE-INPMAQ-X.                                          RMB0003
NOMB01        04 FILLER                PIC X(02)  VALUE "A ".           RMB0003
NOMB01        04 NOMBRE-INPMAQ         PIC X(45)  VALUE SPACES.         RMB0003
NOMB01     03 PARTICULAS-INPMAQ.                                        RMB0003
NOMB01        04 BASURA-INPMAQ            PIC X(45).                    RMB0003
NOMB01        04 P1-INPMAQ                PIC X(45).                    RMB0003
NOMB01        04 P2-INPMAQ                PIC X(45).                    RMB0003
NOMB01        04 P3-INPMAQ                PIC X(45).                    RMB0003
NOMB01        04 P4-INPMAQ                PIC X(45).                    RMB0003
NOMB01        04 P5-INPMAQ                PIC X(45).                    RMB0003
NOMB01        04 P6-INPMAQ                PIC X(45).                    RMB0003
NOMB01        04 P7-INPMAQ                PIC X(45).                    RMB0003
NOMB01        04 P8-INPMAQ                PIC X(45).                    RMB0003
NOMB01        04 P9-INPMAQ                PIC X(45).                    RMB0003
NOMB01        04 P10-INPMAQ               PIC X(45).                    RMB0003
NOMB01 01  SIG-MAQ                     PIC X(1).                        RMB0003
NOMB01     88 SIG-VOCAL-MAQ    VALUE "A" "E" "I" "O" "U".               RMB0003
NOMB01 01  ANT-MAQ                     PIC X(1).                        RMB0003
NOMB01     88 ANT-VOCAL-MAQ    VALUE "A" "E" "I" "O" "U".               RMB0003
NOMB01 01  ACTUAL-MAQ                  PIC X(1).                        RMB0003
NOMB01     88 TILDE-MAQ   VALUE "Á" "É" "Í" "Ó" "Ú" "á" "é" "í" "ó" "ú".RMB0003
NOMB01     88 QUITAR-MAQ  VALUE  "'" "´"  "`" '"' .                     RMB0003
NOMB01     88 BORRAR-MAQ  VALUE "-" "." "1" "2" "3" "4"                 RMB0003
NOMB01                          "5" "6" "7" "8" "9" '0' ")" "," .       RMB0003
NOMB01     88 ELIMINAR-MAQ VALUE "(" .                                  RMB0003
NOMB01     88 VALIDO-MAQ   VALUE "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"RMB0003
NOMB01                           "K" "L" "M" "N" "O" "P" "Q" "R" "S"    RMB0003
NOMB01                           "T" "U" "V" "W" "X" "Y" "Z" " ".       RMB0003
       01 LLAVE-AUX11-BDAMATCH.                 
          02  TIPO-ID-AUX11-BDAMATCH    PIC 9(01).        
          02  NUM-ID-AUX11-BDAMATCH     PIC 9(11).                  
          02  COD-SUS-AUX11-BDAMATCH    PIC 9(06).        
          02  COD-CTA-AUX11-BDAMATCH    PIC 9(02).        
          02  NUM-CTA-AUX11-BDAMATCH    PIC X(18).        
       01 LLAVE-AUX13-BDAMATCH.                 
          02  TIPO-ID-AUX13-BDAMATCH    PIC 9(01).        
          02  NUM-ID-AUX13-BDAMATCH     PIC 9(13).                  
          02  COD-SUS-AUX13-BDAMATCH    PIC 9(06).        
          02  COD-CTA-AUX13-BDAMATCH    PIC 9(02).        
          02  NUM-CTA-AUX13-BDAMATCH    PIC X(18).        
       01 VARIABLES-BDAMATCH.        
PESO01    02 I2-BDAMATCH                PIC 9(2).                       RMB0003
PESO01    02 TIPCTA-BDAMATCH.                                           RMB0003
PESO01*******2=CK,1=CR,2=BS,3=CC,4=OT,9=INVALIDO                        RMB0003
PESO01       03 FILLER PIC X(30) VALUE "211422244444224442444433442444".RMB0003
PESO01       03 FILLER PIC X(30) VALUE "444444444444444444442999999999".RMB0003
PESO01       03 FILLER PIC X(30) VALUE "999999999999999999999999999999".RMB0003
PESO01       03 FILLER PIC X(10) VALUE "9999999999".                    RMB0003
PESO01    02 FILLER REDEFINES TIPCTA-BDAMATCH.                          RMB0003
PESO01       03 V-TIPCTA-BDAMATCH  OCCURS 100 TIMES PIC 9.              RMB0003
PESO01    02 APELLIDO-INPUT-BDAMATCH    PIC X(60) VALUE SPACES.         RMB0003
PESO01    02 ENT-FIN-PORT-BDAMATCH      PIC 9(3)  VALUE 0.              RMB0003
PESO01    02 ENT-UNICAS-PORT-BDAMATCH   PIC 9(3)  VALUE 0.              RMB0003
PESO01    02 ENT-FIN-MEJ-PORT-BDAMATCH  PIC 9(3)  VALUE 0.              RMB0003
VIGE01    02 FALLECIDO-BDAMATCH         PIC 9(01) VALUE 0.              RMB0002
          02 IDE-TIPO-BDAMATCH          PIC 9(01) VALUE 0.        
          02 ABIERTO-BDAMATCH           PIC 9(01) VALUE 0.        
          02 PROCESAR-BDAMATCH          PIC 9(01) VALUE 0.        
          02 FIN-BDAMATCH               PIC 9(01) VALUE 0.        
          02 NUM-IDE-INPUT-BDAMATCH     PIC 9(11).        
          02 TIP-IDE-INPUT-BDAMATCH     PIC 9(01).        
          02 I-GRU-BDAMATCH             PIC 9(5) VALUE 0.        
          02 GRU-NOM-BDAMATCH           PIC 9(5) VALUE 0.        
          02 MOVIDO-BDAMATCH            PIC 9(1) VALUE 0.        
          02 BUSCADO-D-BDAMATCH.        
             03 BUSCADO-BDAMATCH        PIC 9(3) VALUE 0.        
             03 FILLER                  PIC X(1) VALUE " ".        
          02 PORINSERTAR-D-BDAMATCH.        
             03 PORINSERTAR-BDAMATCH    PIC 9(03) VALUE 0.        
             03 FILLER                  PIC X(1) VALUE " ".        
          02 EXISTE-BDAMATCH            PIC 9(05) VALUE 0.        
          02 MEJOR-PORTAFOLIO-BDAMATCH  PIC 9(05) VALUE 0.        
          02 GRU-ANT-BDAMATCH           PIC 9(05) VALUE 0.        
          02 MAX-CUENTAS-BDAMATCH       PIC 9(05) VALUE 5.        
          02 MAX-REL-BDAMATCH           PIC 9(05) VALUE 5.        
          02 MAXREL-PER-BDAMATCH        PIC 9(05) VALUE 0.        
          02 FIN-FUENTE-BDAMATCH        PIC 9(05) VALUE 0.        
          02 FIL-BUS-BDAMATCH           PIC 9(05) VALUE 0.        
          02 COL-BUS-BDAMATCH           PIC 9(05) VALUE 0.        
          02 FIN-BUSCADO-BDAMATCH       PIC 9(05) VALUE 0.        
          02 LIBRE-BUSCADO-BDAMATCH     PIC 9(05) VALUE 0.        
          02 FIL-FUE-BDAMATCH           PIC 9(05) VALUE 0.        
          02 COL-FUE-BDAMATCH           PIC 9(05) VALUE 0.        
          02 I-BDAMATCH                 PIC 9(08) VALUE 0.        
          02 J-BDAMATCH                 PIC 9(08) VALUE 0.        
          02 I-DIR-BDAMATCH             PIC 9(05) VALUE 0.        
          02 I-TEL-BDAMATCH             PIC 9(05) VALUE 0.        
          02 I-NOM-BDAMATCH             PIC 9(05) VALUE 0.        
          02 I-CTA-BDAMATCH             PIC 9(05) VALUE 0.        
          02 NUM-DIR-ANT-BDAMATCH       PIC X(50).        
          02 NUM-TEL-ANT-BDAMATCH       PIC 9(15).        
          02 CTA-DIR-ANT-BDAMATCH       PIC 9(05).        
          02 CTA-TEL-ANT-BDAMATCH       PIC 9(05).        
          02 CTA-NOM-ANT-BDAMATCH       PIC 9(05).        
          02 GRU-NOM-ANT-BDAMATCH       PIC 9(05).        
          02 TABLA-DIR-BDAMATCH.        
             03 T-DIR-BDAMATCH OCCURS 501 TIMES.        
                04 CTA-DIR-BDAMATCH     PIC 9(03).        
                04 NUM-DIR-BDAMATCH     PIC X(50).        
                04 NUM-CTAS-DIR-BDAMATCH PIC 9(03).        
                04 SUS-DIR-BDAMATCH     PIC 9(06).        
                04 LNG-DIR-BDAMATCH     PIC 9(03).        
          02 TABLA-TEL-BDAMATCH.        
             03 T-TEL-BDAMATCH   OCCURS 501 TIMES.        
                04 CTA-TEL-BDAMATCH     PIC 9(03).        
                04 NUM-TEL-BDAMATCH     PIC 9(15).        
                04 NUM-CTAS-TEL-BDAMATCH PIC 9(03).        
                04 SUS-TEL-BDAMATCH     PIC 9(06).        
          02 TABLA-NOM-BDAMATCH.        
             03 T-NOM-BDAMATCH OCCURS 501 TIMES.        
                04 CTA-NOM-BDAMATCH     PIC 9(03).        
                04 NOM-NOM-BDAMATCH     PIC X(60).        
                04 SUS-NOM-BDAMATCH     PIC 9(06).        
                04 GRU-NOMS-BDAMATCH     PIC 9(03).        
                04 LNG-NOM-BDAMATCH     PIC 9(02).        
          02 TABLA-CUENTAS-BDAMATCH.        
             03 T-CUENTAS-BDAMATCH OCCURS 501 TIMES.        
                04 CTA-CUENTAS-BDAMATCH PIC X(38).        
VIGE01          04 CTA-FALLECIDO-BDAMATCH   PIC 9(1).                   RMB0002
FUEN01          04 FUEEXT-CUENTAS-BDAMATCH   PIC 9(1).                  RMB0003
          02 TABLA-GRUNOM-BDAMATCH.        
             03 T-GRUNOM-BDAMATCH OCCURS 501 TIMES.        
                04 GRUPO-GRUNOM-BDAMATCH PIC 9(03).        
                04 NOM-GRUNOM-BDAMATCH   PIC X(60).        
                04 NUM-CTAS-GRUNOM-BDAMATCH PIC 9(03).        
                04 LNG-GRUNOM-BDAMATCH   PIC 9(02).        
APEL01          04 APE-GRUNOM-BDAMATCH   PIC 9(03).                     RMB0003
          02  WS-PESOS-CUENTAS-BDAMATCH.        
            03  ESTA-CTA-BDAMATCH        PIC  9(01)  VALUE  ZEROS.        
            03  INV-BDAMATCH             PIC 9(04) VALUE ZEROS.        
            03  SUSC-CTAS-BDAMATCH       PIC 9(06) VALUE ZEROS.        
            03  SUSC-GRUPO-BDAMATCH.        
              05  SUSC-GRUP-SUS-BDAMATCH PIC  9(06)  VALUE  ZEROS.        
              05  SUSC-GRUP-GRU-BDAMATCH PIC  9(03)  VALUE  ZEROS.        
            03  PESO-CTAS-TOT-BDAMATCH   PIC 9(12) VALUE ZEROS.        
            03  PESOS-CTRL-BDAMATCH      PIC 9(12) VALUE ZEROS.        
            03  NOM-LIMPI-BDAMATCH       PIC X(60) VALUE SPACES.        
            03  CTA-A-BUSCAR-BDAMATCH    PIC 9(03) VALUE ZEROS.        
            03  PESO-TEL-BDAMATCH        PIC 9(02) VALUE 1.        
            03  PESO-DIR-BDAMATCH        PIC 9(02) VALUE 3.        
            03  PESO-NOM-BDAMATCH        PIC 9(02) VALUE 9.        
            03  POSIC-TEL-BDAMATCH       PIC 9(02) VALUE ZEROS.        
            03  POSIC-DIR-BDAMATCH       PIC 9(02) VALUE ZEROS.        
            03  POSIC-NOM-BDAMATCH       PIC 9(02) VALUE ZEROS.        
            03  POSIC-GRU-BDAMATCH       PIC 9(02) VALUE ZEROS.        
            03  NRO-VECES-1-BDAMATCH     PIC 9(03) VALUE ZEROS.        
            03  NRO-VECES-2-BDAMATCH     PIC 9(03) VALUE ZEROS.        
            03  NRO-VECES-3-BDAMATCH     PIC 9(03) VALUE ZEROS.        
            03  POSIC-SUMA-BDAMATCH      PIC 9(03) VALUE ZEROS.        
            03  POSIC-SUSC-BDAMATCH      PIC 9(03) VALUE ZEROS.        
            03  POSIC-SUS-GRU-BDAMATCH   PIC 9(03) VALUE ZEROS.        
       01 V-INTERRELAC-BDAMATCH.        
          02 CARAC-REL-BDAMATCH  OCCURS 501 TIMES PIC X(03).        
          02 FILA-REL-BDAMATCH OCCURS 501 TIMES.        
             03 COLUMNA-REL-BDAMATCH OCCURS 501 TIMES.        
                04 CAJA-BDAMATCH         PIC 9(03).        
                04 DELIMITADOR-BDAMATCH  PIC X(01).        
       01 VARIABLES-NOMBRES-BDAMATCH.        
          03 LIM-BDAMATCH                PIC  9(03) VALUE 500.        
          03 DIR-INPUT-BDAMATCH          PIC  X(60).        
          03 DIR-OUTPUT-BDAMATCH         PIC  X(60).        
          03 CTA-INPUT-BDAMATCH          PIC  X(38).        
          03 TEL-INPUT-BDAMATCH          PIC  9(15).        
          03 TEL-OUTPUT-BDAMATCH         PIC  9(15).        
          03 NOM-INPUT-BDAMATCH          PIC  X(60).        
          03 NOM-OUTPUT-BDAMATCH         PIC  X(60).        
VIGE01    03 NUM-MUERTOS-OUTPUT-BDAMATCH PIC  9(03).                    RMB0002
VIGE01    03 NUM-CTAS-OUTPUT-BDAMATCH    PIC  9(03).                    RMB0002
          03 VALCTA-OUTPUT-BDAMATCH      PIC  9(01).        
          03 CAR-ANT-BDAMATCH            PIC  X(01).                        
          03 CAR-ACT-BDAMATCH            PIC  9(01).                         
          03 T-CARDIR-BDAMATCH.                                                 
             04 V-CARDIR-BDAMATCH       OCCURS 60 TIMES PIC X(01).        
          03 T-CARTOT-BDAMATCH.                                              
             04 V-CARTOT-BDAMATCH       OCCURS 10 TIMES PIC 9(05).        
       01  TABLA-SUSC-DIR-BDAMATCH.                                          
         03  REG-SUSC-DIR-BDAMATCH   OCCURS 501 TIMES              
             ASCENDING KEY IS COD-SUSC-DIR-BDAMATCH  INDEXED BY         
                                                     IND-DIR-BDAMATCH.        
           05  COD-SUSC-DIR-BDAMATCH     PIC 9(06).                    
           05  FLAG-YA-CTA-BDAMATCH      PIC 9(01).                    
           05  FLAG-YA-DIR-BDAMATCH      PIC 9(01).                    
           05  FLAG-YA-TEL-BDAMATCH      PIC 9(01).                    
           05  FLAG-YA-NOM-BDAMATCH      PIC 9(01).                    
       01  TABLA-SUSC-NOMBRE-BDAMATCH.        
         03  REG-SUSC-NOM-BDAMATCH        OCCURS 501 TIMES        
             ASCENDING  KEY  IS  KEY-SUSC-NOM-BDAMATCH  INDEXED         
                                 BY IND-NOM-BDAMATCH.        
           05  KEY-SUSC-NOM-BDAMATCH.        
             07  COD-SUSC-NOM-BDAMATCH   PIC 9(06).        
             07  GRUPO-SUS-NOM-BDAMATCH  PIC 9(03).        
