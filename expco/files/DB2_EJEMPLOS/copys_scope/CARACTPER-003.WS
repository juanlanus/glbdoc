REMARK*CARACTPER-003.WS  .v.0003
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA CITIBANK       
REMARK**********************************************************************
REMARK* CAV0002     | CQ8603 - SE INCLUYE ARCHIVO CITI CLIENTES FERIA  *
REMARK* 31-OCT-2012 |                                                  *
REMARK******************************************************************
REMARK* IMS0003     | R10704 - SE AGREGA LA CARACTERISTICA EXTERNA     *
REMARK* 26-FEB-2013 |          TMCITIFER003                            *
REMARK******************************************************************
        01 VARIABLES-PER-003.
           03 SW-INQ-003                PIC 9(01)   VALUE ZEROS.
           03 SUSCRIPTOR-003            PIC 9(06)   VALUE ZEROS.
           03 MESES-MADUR-CARCIT        PIC S9(05)  VALUE ZEROS.
           03 MESES-MADUR-CARCIT2       PIC S9(05)  VALUE ZEROS.
           03 MESES-DESAC-CARCIT        PIC S9(05)  VALUE ZEROS.
           03 MESES-DESAC-CARCIT2       PIC S9(05)  VALUE ZEROS.
           03 MESES-DESAC-CARCIT3       PIC S9(05)  VALUE ZEROS.
           03 MESES-NOVAP-CARCIT2       PIC S9(05)  VALUE ZEROS.
           03 MESES-CONST-CARCIT        PIC S9(05)  VALUE ZEROS.
           03 MESES-ADJTV-MENOR         PIC S9(05)  VALUE ZEROS.
           03 MESES-ADJTV-MAYOR         PIC S9(05)  VALUE ZEROS.
           03 MESES-ULT-PAGOTC          PIC S9(05)  VALUE ZEROS.
           03 WS-VAL989                 PIC S9(05)  VALUE ZEROS.
           03 WS-TOL-CITI               PIC 9(01)   VALUE ZEROS.
           03 V-NOR-CARACT-003          PIC 9(01)  VALUE ZEROS.
           03 WS-MORAS-180              PIC 9(03)  VALUE ZEROS.
           03 WS-FINCU                  PIC S9(05)  VALUE ZEROS.
           03 WS-NN                     PIC 9(02)  VALUE ZEROS.
           03 WS-NN-1                   PIC 9(02)  VALUE ZEROS.
           03 WS-NN-2                   PIC 9(02)  VALUE ZEROS.
           03 WS-PORIN                  PIC 999V99999  VALUE ZEROS.
           03 WS-UTILIZATO              PIC 999V99999  VALUE ZEROS.
           03 WS-PRODS-C                PIC 9(03)   VALUE ZEROS.
           03 WS-PRODS-TDC              PIC 9(03)   VALUE ZEROS.
           03 WS-003-VAR1               PIC 9(03)   VALUE ZEROS.
           03 WS-003-VAR2               PIC 9(03)   VALUE ZEROS.
           03 WS-003-MINMO-R.                                       
           04 WS-003-MINMO              PIC 9(05)   VALUE ZEROS.
           03 WS-003-MINPO              PIC 9(03)   VALUE ZEROS.
           03 WS-003-MINTO              PIC 9(03)   VALUE ZEROS.
           03 WS-003-MOB13              PIC 9(05)   VALUE ZEROS.
           03 WS-003-MAXMOB13           PIC 9(05)   VALUE ZEROS.
           03 WS-003-DELQ-R.                                       
           04 WS-003-DELQ               PIC 9(05)   VALUE ZEROS.
           03 WS-003-MINDELQ-R.                                   
           04 WS-003-MINDELQ            PIC 9(05)   VALUE ZEROS.
           03 WS-003-NN                 PIC 9(02)   VALUE ZEROS.
           03 WS-FLAG-TDC               PIC 9(01)   VALUE ZEROS.
           03 WS-FLAG-CITI              PIC 9(01)   VALUE ZEROS.
           03 SW-PROCESAR-003           PIC 9(01)   VALUE ZEROS.
           03 FLAG-VALORES-CARCIT       PIC 9(01)   VALUE ZEROS. 
           03 CUPO-CARCIT               PIC 9(11)   VALUE ZEROS.
           03 SALDO-CARCIT              PIC 9(11)   VALUE ZEROS.
           03 CUOTA-CARCIT              PIC 9(11)   VALUE ZEROS.
           03 CUOTAS-CARCIT             PIC 9(05)   VALUE ZEROS.
           03 FLAG-END985               PIC 9(01)   VALUE ZEROS.
           03 FLAG-NONCITI              PIC 9(01)   VALUE ZEROS.
           03 FLAG-OPNOCITI             PIC 9(01)   VALUE ZEROS.
           03 RO-VALUE                  PIC 999V99999  VALUE ZEROS.
           03 RO-CUPOS                  PIC 9(11)   VALUE ZEROS.
           03 RO-SALDOS                 PIC 9(11)   VALUE ZEROS.
           03 RO-01SALDO                PIC 9(11)   VALUE ZEROS.
           03 CC-FLAG                   PIC 9(01)   VALUE ZEROS.
           03 RO-01FLAG                 PIC 9(01)   VALUE ZEROS.
           03 RO-01FLAG-CUO             PIC 9(01)   VALUE ZEROS.
           03 RO-01FLAG-996             PIC 9(01)   VALUE ZEROS.
           03 RO-01FLAG-EXP             PIC 9(01)   VALUE ZEROS.
           03 RO-01FLAG-EXP-996         PIC 9(01)   VALUE ZEROS.
           03 RO-01FLAG-988             PIC 9(01)   VALUE ZEROS.
           03 RO-01FLAG-987             PIC 9(01)   VALUE ZEROS.
           03 RO-01FLAG-986             PIC 9(01)   VALUE ZEROS.
           03 RO-03FLAG                 PIC 9(01)   VALUE ZEROS.
           03 R1-01FLAG                 PIC 9(01)   VALUE ZEROS.
           03 R1-01FLAG-CUO             PIC 9(01)   VALUE ZEROS.
           03 R1-01FLAG-EXP             PIC 9(01)   VALUE ZEROS.
           03 R1-01FLAG-996             PIC 9(01)   VALUE ZEROS.
           03 IN-01FLAG                 PIC 9(01)   VALUE ZEROS.
           03 IN-01FLAG-EXP             PIC 9(01)   VALUE ZEROS.
           03 IN-01FLAG-CUO             PIC 9(01)   VALUE ZEROS.
           03 IN-01FLAG-996             PIC 9(01)   VALUE ZEROS.
           03 VE-01FLAG                 PIC 9(01)   VALUE ZEROS.
           03 VE-01FLAG-EXP             PIC 9(01)   VALUE ZEROS.
           03 VE-01FLAG-CUO             PIC 9(01)   VALUE ZEROS.
           03 HP-01FLAG                 PIC 9(01)   VALUE ZEROS.
           03 HP-01FLAG-EXP             PIC 9(01)   VALUE ZEROS.
           03 HP-01FLAG-CUO             PIC 9(01)   VALUE ZEROS.
           03 MX-01FLAG-EXP-994         PIC 9(01)   VALUE ZEROS.
           03 MX-01FLAG-MOR             PIC 9(01)   VALUE ZEROS.
           03 MX-01FLAG-NUM             PIC 9(01)   VALUE ZEROS.
           03 MX-01FLAG-CERR            PIC 9(01)   VALUE ZEROS.
           03 MX-01FLAG-984             PIC 9(01)   VALUE ZEROS.
           03 MX-02FLAG                 PIC 9(01)   VALUE ZEROS.
           03 MX-02FLAG-MOR             PIC 9(01)   VALUE ZEROS.
           03 MX-02FLAG-MOR989          PIC 9(01)   VALUE ZEROS.
           03 MX-02FLAG-993             PIC 9(01)   VALUE ZEROS.
           03 MX-02FLAG-992             PIC 9(01)   VALUE ZEROS.
           03 MX-02FLAG-991             PIC 9(01)   VALUE ZEROS.
           03 MX-02FLAG-990             PIC 9(01)   VALUE ZEROS.
           03 MX-02FLAG-989             PIC 9(01)   VALUE ZEROS.
           03 MX-02FLAG-981             PIC 9(01)   VALUE ZEROS.
           03 INQFLAG-3M                PIC 9(01)   VALUE ZEROS.
           03 INQFLAG-997               PIC 9(01)   VALUE ZEROS.
           03 INQFLAG-995               PIC 9(01)   VALUE ZEROS.
           03 INQFLAG                   PIC 9(01)   VALUE ZEROS.
           03 CT-FLAG                   PIC 9(01)   VALUE ZEROS.
           03 AH-FLAG                   PIC 9(01)   VALUE ZEROS.
           03 TIPO3-FLAG                PIC 9(01)   VALUE ZEROS.
           03 TIPO3-FLAG-RO             PIC 9(01)   VALUE ZEROS.
           03 RO987-SALDOS              PIC 9(11)   VALUE ZEROS.
           03 RO987-CUPOS               PIC 9(11)   VALUE ZEROS.
           03 WS-INICIO12               PIC 9(02)   VALUE ZEROS.
           03 WS-INICIO24               PIC 9(02)   VALUE ZEROS.
           03 WS-ACTUAL-CITI            PIC 9(01)   VALUE ZEROS.
           03 WS-ANT-CITI               PIC 9(01)   VALUE ZEROS.
           03 WS-ANT2-CITI              PIC 9(01)   VALUE ZEROS.
           03 GARANTE-CITI              PIC 9(02)   VALUE ZEROS.
           03 FECHA-AUX-003             PIC 9(04)   VALUE ZEROS.
           03 FECHA-NOV-003             PIC 9(06)   VALUE ZEROS.
           03 FECHA-NOV-003-R REDEFINES  FECHA-NOV-003.
              05 AA-NOV-003             PIC 9(04).
              05 MM-NOV-003             PIC 9(02).
           03 FECHA-MEN-003             PIC 9(06)   VALUE ZEROS.
           03 FECHA-MEN-003-R REDEFINES  FECHA-MEN-003.
              05 AA-MEN-003             PIC 9(04).
              05 MM-MEN-003             PIC 9(02).
           03 FECHA-MAY-003             PIC 9(06)   VALUE ZEROS.
           03 FECHA-MAY-003-R REDEFINES  FECHA-MAY-003.
              05 AA-MAY-003             PIC 9(04).
              05 MM-MAY-003             PIC 9(02).
           03 WS-ENTIDAD-CITI           PIC 9(11)  VALUE ZEROS.
      *       88 ENT-CITI              VALUE 860051135.
           03 WS-ENTSUSC-CITI           PIC X(02)  VALUE SPACES.
              88 ENT-CITI              VALUE "SI".         
           03 WS-ENTSUSC-CITI-B         PIC X(02)  VALUE SPACES.
              88 ENT-CITI-B            VALUE "SI".
           03 WS-ENTSUSC-CITI-PAR       PIC X(02)  VALUE SPACES.
              88 ENT-CITI-PAR          VALUE "SI".         
           03 WS-ENTSUSC-CITI-CIU       PIC X(02)  VALUE SPACES.
              88 ENT-CITI-CIUD         VALUE "SI".         
           03 I-IND-C                   PIC 9(03)  VALUE ZEROS.
           03 I-IND-B                   PIC 9(03)  VALUE ZEROS.
           03 I-IND-CIU                 PIC 9(03)  VALUE ZEROS.
           03 I-IND-PAR                 PIC 9(03)  VALUE ZEROS.
           03 W-SUSC-CITI.
              05 W-SUS-CITI-T           PIC 9(01)  VALUE ZEROS.
              05 W-SUS-CITI-S           PIC 9(06)  VALUE ZEROS.
           03 II-TP                     PIC 9(03)  VALUE ZEROS.
      *  VALORES AUXILIARES PARA ACUMULAR Y REDONDEAR
           03 01END997RO003-AUX       PIC 9(11)V9(05) VALUE ZEROS.
           03 01END997R1003-AUX       PIC 9(11)V9(05) VALUE ZEROS.
           03 01END997IN003-AUX       PIC 9(11)V9(05) VALUE ZEROS.
           03 01END997VE003-AUX       PIC 9(11)V9(05) VALUE ZEROS.
           03 01END997HP003-AUX       PIC 9(11)V9(05) VALUE ZEROS.
           03 01END990MX003-AUX       PIC 9(11)V9(05) VALUE ZEROS.
           03 01END989MX003-AUX       PIC 9(11)V9(05) VALUE ZEROS.
      * CAMPOS PARA LA NUEVA bESSION DE 01END987XX003
           03 FLAG-SDO-003-987          PIC X     VALUE SPACES.
           03 FLAG-VLR-003-987          PIC X     VALUE SPACES.
           03 I-C-003                   PIC 9(05) VALUE ZEROS.
      
      
        01 TABLA-SUSC-CITI.
           03 TB-SUSC-CITI OCCURS 20 ASCENDING KEY IS SUS-CITI
                                      INDEXED BY IND-S.
              05 SUS-CITI               PIC 9(06).                
           03 TB-SUSC-CITI-B OCCURS 200 ASCENDING KEY IS SUS-CITI-B
                                      INDEXED BY IND-B.
              05 SUS-CITI-B             PIC 9(06).                
           03 TB-SUSC-CITI-PAR OCCURS 10 ASCENDING KEY IS SUS-CITI-PAR
                                      INDEXED BY IND-P.
              05 SUS-CITI-PAR           PIC 9(06).                
           03 TB-SUSC-CITI-CIU OCCURS 10 ASCENDING KEY IS SUS-CITI-CIU
                                      INDEXED BY IND-C.
              05 SUS-CITI-CIU           PIC 9(06).                
      
        01 PORTAF-PER-003.   
           03 COD-CTA-PORTAF           PIC 9(02).
      *       88 SECTOR-FINANCIERO VALUE 01 02 03 05 07 13 14 17
      *                                  18 20 48 49 51.
      *       88 PORTAF-MX-01 VALUE    02 05 06 07 13 15 16 18 27 
      *                                44 45 46.
      *       88 PORTAF-MX-02 VALUE    02 03 05 06 07 27 18 14 15
      *                                17 04 45 13 48.
      *       88 PORTAF-AH-01 VALUE    51.
      *       88 PORTAF-CT-01 VALUE    44.
      *       88 PORTAF-RO-01 VALUE    02.
      *       88 PORTAF-RO-02 VALUE    06 27.
      *       88 PORTAF-RO-03 VALUE    02 03 48.
      *       88 PORTAF-R1-01 VALUE    03 48.
      *       88 PORTAF-VE-01 VALUE    04 45.
      *       88 PORTAF-VE-02 VALUE    18.    
      *       88 PORTAF-IN-01 VALUE    05 06 07 27 18 14 15 17.
      *       88 PORTAF-IN-02 VALUE    46.                      
      *       88 PORTAF-I1-02 VALUE    05.                      
      *       88 PORTAF-HP-01 VALUE    13.
      *       88 PORTAF-HP-02 VALUE    13 15 16.
      *       88 PORTAF-HP-03 VALUE    07.
      *       88 PORTAF-CC    VALUE    23 24. 
      *       88 PORTAF-OT-01 VALUE    04 44 45.
           03 SW-FACTOR-CITI-CAL       PIC 9(01) VALUE 0.
           03 FACTOR-CITI              PIC 9(01)V9(04).
           03 SW-FACTOR-CITI           PIC 9(01) VALUE 0.
           03 NM-FACTOR-CITI           PIC X(05) VALUE SPACES.
           03 SW-SUSCR-CITI            PIC 9(01) VALUE 0.
              
        01 TABLA-FACTOR-CITI.      
           02 T-FACTOR-CITI   OCCURS 20 TIMES.
              03 COD-PORTAF-C           PIC X(05).
              03 NUM-CUOTAS-C           PIC 9(03).
              03 VAL-FACTOR-C           PIC 9(02)V9(02).
           02 I-FACTOR-C                PIC 9(03) VALUE 0.
      
      
        01 TABLA-ENTIDADES-CITI.
      *    03 TAB-ENTID-CITI OCCURS 100 TIMES.
           03 TAB-ENTID-CITI OCCURS 1000 TIMES.
              05 NIT-ENTID-CITI         PIC 9(11).
              05 NUM-C-PRODS            PIC 9(04).
              05 SUMA-C-RO              PIC 9(11).
              05 SUMA-C-R1              PIC 9(11).
              05 SUMA-C-CC              PIC 9(11).
              05 SUMA-C-CT              PIC 9(11).
              05 SUMA-C-AH              PIC 9(11).
              05 SUMA-C-IN              PIC 9(11).
              05 SUMA-C-HP              PIC 9(11).
              05 SUMA-C-VE              PIC 9(11).
              05 SUMA-C-CO              PIC 9(11).
              05 SUMA-C-OT              PIC 9(11).
              05 SUMA-C-RO-03           PIC 9(11).
              05 SUMA-S-RO              PIC 9(11).
              05 SUMA-S-R1              PIC 9(11).
              05 SUMA-S-CC              PIC 9(11).
              05 SUMA-S-CT              PIC 9(11).
              05 SUMA-S-AH              PIC 9(11).
              05 SUMA-S-IN              PIC 9(11).
              05 SUMA-S-HP              PIC 9(11).
              05 SUMA-S-VE              PIC 9(11).
              05 SUMA-S-CO              PIC 9(11).
              05 SUMA-S-OT              PIC 9(11).
              05 SUMA-S-RO-03           PIC 9(11).
              05 NUM-C-RO               PIC 9(04).
              05 NUM-C-R1               PIC 9(04).
              05 NUM-C-IN               PIC 9(04).
           03 IND-ENTIDAD               PIC 9(03) VALUE ZEROS.
           03 SUMA-CUPO                 PIC 9(11) VALUE ZEROS.
           03 SAL-TC-NONC               PIC 9(11) VALUE ZEROS.
           03 SAL-TC-CONC               PIC 9(11) VALUE ZEROS.
           03 SAL-TC-CONC-1             PIC 9(11) VALUE ZEROS.
           03 CUPO-TC-NONC              PIC 9(11) VALUE ZEROS.
           03 CUPO-TC-CONC              PIC 9(11) VALUE ZEROS.
           03 CUPO-TC-CONC-1            PIC 9(11) VALUE ZEROS.
           03 UTLZ-TC-NONC              PIC 9(03)V9(03) VALUE ZEROS.
           03 UTLZ-TC-CONC              PIC 9(03)V9(03) VALUE ZEROS.
           03 UTLZ-TC-CONC-1            PIC 9(03)V9(03) VALUE ZEROS.
      
        01 TABLA-ENT-CONSL-CITI.
           03 TAB-ENT-CS-CITI OCCURS 100 TIMES.
              05 NIT-ENT-CS-CITI        PIC 9(11).
              05 NUM-C-3M               PIC 9(04).
              05 NUM-C-3M-P             PIC 9(04).
              05 NUM-C-6M               PIC 9(04).
              05 NUM-C-6M-P             PIC 9(04).
              05 NUM-C-6M-NOCITI        PIC 9(04).
      
        01 TABLA-985-PORTA-CITI.
           03 TAB-ENT-CS-CITI OCCURS 6   TIMES.
              05 END985-SALDOS          PIC 9(11).
              05 END985-CUPOS           PIC 9(11).
              05 END985-MAXPOR          PIC 9(03)V9(05).
      
        01 TABLA-END987-003.
           03 TB-END987-003  OCCURS 200 ASCENDING KEY IS LLAVE-END987
                                      INDEXED BY IND-987.
             04  LLAVE-END987.
              05 FECHA-AP-987           PIC 9(06).
              05 FECHA-UL-987           PIC 9(06).
              05 CONSECUTIVO-987        PIC 9(05).
             04  SALDO-987-003          PIC X(11).
             04  SALDO-987-003-R REDEFINES SALDO-987-003   PIC 9(11).
             04  CUPO-987-003           PIC X(11).
             04  CUPO-987-003-R  REDEFINES CUPO-987-003    PIC 9(11).
      
      
        01 PERSONALIZACION-03.
           02 PER-03-NIVEL-01-END.
              03 01END999XX003.
                 04 01END999RO003       PIC 9(11).
                 04 01END999R1003       PIC 9(11).
                 04 01END999IN003       PIC 9(11).
                 04 01END999VE003       PIC 9(11).
                 04 01END999HP003       PIC 9(11).
              03 01END998XX003.
                 04 01END998RO003       PIC 9(11).
                 04 01END998R1003       PIC 9(11).
                 04 01END998IN003       PIC 9(11).
                 04 01END998VE003       PIC 9(11).
                 04 01END998HP003       PIC 9(11).
              03 01END997XX003.
                 04 01END997RO003       PIC 9(11).
                 04 01END997R1003       PIC 9(11).
                 04 01END997IN003       PIC 9(11).
                 04 01END997VE003       PIC 9(11).
                 04 01END997HP003       PIC 9(11).
              03 01END996XX003.
                 04 01END996RO003       PIC 9(11).
                 04 01END996R1003       PIC 9(11).
                 04 01END996IN003       PIC 9(11).
              03 01END995XX003.
                 04 01END995IN003       PIC 9(11).
              03 01END994XX003.
                 04 01END994RO003       PIC 9(11).
                 04 01END994R1003       PIC 9(11).
              03 01END993XX003.
                 04 01END993MX003       PIC 9(11).
              03 01END992XX003.
                 04 01END992MX003       PIC 9(11).
              03 01END991XX003.
                 04 01END991MX003       PIC 9(11).
              03 01END990XX003.
                 04 01END990MX003       PIC 9(11).
              03 01END989XX003.
                 04 01END989MX003       PIC 9(11).
              03 01END988XX003.
                 04 01END988RO003       PIC 9(03)V9(05).
              03 01END987XX003.
                 04 01END987RO003       PIC 9(03)V9(05).
              03 01END986XX003.
                 04 01END986RO003       PIC 9(03)V9(05).
              03 01END984XX003.
                 04 01END984RO003       PIC 9(03)V9(05).
              03 01END981XX003.
                 04 01END981MX003       PIC 9(11).
      
           02 PER-03-NIVEL-01-EXP.
              03 01EXP999XX003.
                 04 01EXP999AH003       PIC 9(03).
                 04 01EXP999CT003       PIC 9(03).
                 04 01EXP999RO003       PIC 9(03).
                 04 01EXP999R1003       PIC 9(03).
                 04 01EXP999VE003       PIC 9(03).
                 04 01EXP999IN003       PIC 9(03).
                 04 01EXP999HP003       PIC 9(03).
              03 01EXP996XX003.
                 04 01EXP996RO003       PIC 9(03).
              03 01EXP994XX003.
                 04 01EXP994MX003       PIC 9(03).
                 
           02 PER-03-NIVEL-01-EXT.
              03 01EXT999XX003.
                 04 01EXT999CC003       PIC 9(01).
              03 01EXT998XX003.
                 04 01EXT998CC003       PIC 9(01).
              03 01EXT997XX003.
                 04 01EXT997CC003       PIC 9(01).
              03 01EXT996XX003.
                 04 01EXT996CC003       PIC 9(01).
              03 01EXT995XX003.
                 04 01EXT995CC003       PIC 9(01).
              03 01EXT994XX003.
                 04 01EXT994HP003       PIC 9(03).
              03 01EXT993XX003.
                 04 01EXT993RO003       PIC 9(01).
              03 01EXT992XX003.
                 04 01EXT992MX003       PIC 9(01).
              03 01EXT990XX003.
                 04 01EXT990MX003       PIC 9(01).
                 
           02 PER-03-NIVEL-01-MOR.
              03 01MOR999XX003.
                 04 01MOR999MX003       PIC 9(03).
              03 01MOR998XX003.
                 04 01MOR998MX003       PIC 9(03).
              03 01MOR997XX003.
                 04 01MOR997MX003       PIC 9(03).
              03 01MOR996XX003.
                 04 01MOR996MX003       PIC 9(03).
              03 01MOR995XX003.
                 04 01MOR995MX003       PIC 9(03).
              03 01MOR994XX003.
                 04 01MOR994MX003       PIC 9(03).
              03 01MOR993XX003.
                 04 01MOR993MX003       PIC 9(03).
              03 01MOR992XX003.
                 04 01MOR992MX003       PIC 9(03).
              03 01MOR991XX003.
                 04 01MOR991MX003       PIC 9(03).
              03 01MOR990XX003.
                 04 01MOR990MX003       PIC 9(03).
              03 01MOR989XX003.
                 04 01MOR989MX003       PIC 9(03).
              03 01MOR988XX003.
                 04 01MOR988MX003       PIC 9(03).
              03 01MOR986XX003.
                 04 01MOR986MX003       PIC 9(03).
              03 01MOR985XX003.
                 04 01MOR985CC003       PIC 9(03).
              03 01MOR984XX003.
                 04 01MOR984MX003       PIC 9(03).
              03 01MOR983XX003.
                 04 01MOR983MX003       PIC 9(03).
              03 01MOR980XX003.
                 04 01MOR980MX003       PIC 9(03).
      
           02 PER-03-NIVEL-01-NUM.
              03 01NUM999XX003.
                 04 01NUM999MX003       PIC 9(04).  *>  O ES MX ??
              03 01NUM998XX003.
                 04 01NUM998MX003       PIC 9(04).  *>  O ES MX ??
              03 01NUM997XX003.
                 04 01NUM997CT003       PIC 9(04).  *>  O ES MX ??
              03 01NUM996XX003.
                 04 01NUM996MX003       PIC 9(04).  *>  
              03 01NUM995XX003.
                 04 01NUM995MX003       PIC 9(04).  *>  
              03 01NUM994XX003.
                 04 01NUM994RO003       PIC 9(04).  *>  
                 04 01NUM994R1003       PIC 9(04).  *>  
                 04 01NUM994IN003       PIC 9(04).  *>  
              03 01NUM993XX003.
                 04 01NUM993MX003       PIC 9(04).  *>  
              03 01NUM991XX003.
                 04 01NUM991MX003       PIC 9(04).  *>  
              03 01NUM990XX003.
                 04 01NUM990RO003       PIC 9(04).  *>  
              03 01NUM989XX003.
                 04 01NUM989MX003       PIC 9(04).  *>  
                 
           02 PER-03-NIVEL-02-END.
              03 02END985-003           PIC 9(03)V9(05).
              03 02END983XX003.
                 04 02END983RO003       PIC 9(11).
              03 02END982XX003.
                 04 02END982RO003       PIC 9(11).
                 
           02 PER-03-NIVEL-02-EXP.
              03 02EXP998-003           PIC 9(03).
              03 02EXP997-003           PIC 9(03).
              03 02EXP995-003           PIC 9(03).
                 
                 
           02 PER-03-NIVEL-02-INQ.
              03 02INQ999-003           PIC 9(04).
              03 02INQ998-003           PIC 9(04).
              03 02INQ997-003           PIC 9(04).
              03 02INQ996-003           PIC 9(04).
              03 02INQ995-003           PIC 9(04).
                 
           02 PER-03-NIVEL-02-MOR.
              03 02MOR987-003           PIC 9(03).
              03 02MOR982-003           PIC 9(03).
              03 02MOR981-003           PIC 9(03).
              03 02MOR979-003           PIC 9(03).
                 
           02 PER-03-NIVEL-02-NUM.
              03 02NUM992-003           PIC 9(04).
              03 02NUM988-003           PIC 9(04).
              03 02NUM987-003           PIC 9(04).
              03 02NUM986-003           PIC 9(04).
              03 02NUM985-003           PIC 9(04).
              03 02NUM984-003           PIC 9(04).
      *   NUEVO  
           02 NUEVOS.
              03 02EXT991-003            PIC 9(01).
              03 02EXT989-003            PIC X(03).
              03 02EXT988-003            PIC X(08).
              03 02EXT987-003            PIC X(11).
              03 02EXT986-003            PIC X(11).
              03 02EXT985-003            PIC X(11).
              03 02EXT984-003            PIC 9(06)V9(05).
              03 02EXT984-003-R REDEFINES 02EXT984-003 PIC X(11).
              03 02EXT983-003            PIC 9(06)V9(05).
              03 02EXT983-003-R REDEFINES 02EXT983-003 PIC X(11).
              03 02EXT982-003            PIC 9(06)V9(05).
              03 02EXT982-003-R REDEFINES 02EXT982-003 PIC X(11).
              03 02EXT981-003            PIC 9(06)V9(05).
              03 02EXT981-003-R REDEFINES 02EXT981-003 PIC X(11).
              03 02EXT980-003            PIC X(11).
              03 02EXT979-003            PIC X(11).
              03 02EXT978-003            PIC X(11).
              03 02EXT977-003            PIC X(11).
              03 02EXT976-003            PIC X(11).
              03 02EXT975-003            PIC X(11).
              03 02EXT974-003            PIC X(11).
              03 02EXT973-003            PIC X(11).
              03 02EXT972-003            PIC X(11).
              03 02EXT971-003            PIC X(11).
              03 02EXT970-003            PIC X(11).
              03 02EXT969-003            PIC X(11).
              03 02EXT968-003            PIC X(11).
              03 02EXT967-003            PIC X(03).
              03 02EXT966-003            PIC X(11).
              03 02EXT965-003            PIC X(11).
              03 02EXT964-003            PIC X(11).
              03 02EXT963-003            PIC X(11).
              03 02EXT962-003            PIC X(11).
              03 02EXT961-003            PIC X(11).
              03 02EXT960-003            PIC X(11).
              03 02EXT959-003            PIC X(11).
              03 02EXT958-003            PIC X(11).
CQ8603     02 VAR-CITI-FERIA-003.                                       CAV0002
CQ8603        03 INDCLIFER003            PIC 9(01).                     CAV0002
CQ8603        03 FLAGAGFER003            PIC X(02).                     CAV0002
CQ8603        03 TIPPREFER003            PIC 9(01).                     CAV0002
CQ8603        03 HELLONFER003            PIC X(02).                     CAV0002
icimsd        03 OFERTAFER003            PIC 9(13).                     CAV0002
icimsd*CQ8603 03 OFERTAFER003            PIC 9(15).                     CAV0002
R10704        03 TMCITIFER003            PIC X(02).                     IMS0003
