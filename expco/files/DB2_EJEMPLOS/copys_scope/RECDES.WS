REMARK*CARDES-WS.0001.
REMARK******************************************************************
REMARK* DEFINICIO DE VARIABLES DE LA RUTINA RECDES.PROC
REMARK******************************************************************
        01 VAR-RECDES.
           02 INICIO-RECDES         PIC 9(01) VALUE 0.
           02 FIN-BUSCAR-RECDES     PIC 9(01) VALUE 0.
           02 FIN-ICBDIM-RECDES     PIC 9(01) VALUE 0.
           02 TIPIDE-RECDES         PIC 9(01) VALUE 0.
           02 NUMIDE-RECDES         PIC 9(11) VALUE 0.
           02 W-VALCAR-RECDES       PIC X(60) VALUE SPACES.
           02 I-RECDES              PIC 9(06) VALUE 0.      
           02 J-RECDES              PIC 9(06) VALUE 0.      
           02 STRING-RECDES         PIC X(2000) VALUE SPACES. 
        01 NOMCAR-RECDES.
           02 FILLER               PIC X(11) VALUE "C02SCR013TO".
*
           02 FILLER               PIC X(11) VALUE "C00DEM035TO".
           02 FILLER               PIC X(11) VALUE "C00DEM036TO".
           02 FILLER               PIC X(11) VALUE "C00DEM037TO".
           02 FILLER               PIC X(11) VALUE "C00DEM038TO".
           02 FILLER               PIC X(11) VALUE "C00DEM039TO".
           02 FILLER               PIC X(11) VALUE "C00DEM040TO".
           02 FILLER               PIC X(11) VALUE "C00DEM041TO".
           02 FILLER               PIC X(11) VALUE "C00DEM042TO".
           02 FILLER               PIC X(11) VALUE "C00DEM043TO".
           02 FILLER               PIC X(11) VALUE "C00DEM044TO".
           02 FILLER               PIC X(11) VALUE "C00DEM045TO".
*
           02 FILLER               PIC X(11) VALUE "C00DEM046TO".
           02 FILLER               PIC X(11) VALUE "C00DEM047TO".
           02 FILLER               PIC X(11) VALUE "C00DEM048TO".
           02 FILLER               PIC X(11) VALUE "C00DEM049TO".
           02 FILLER               PIC X(11) VALUE "C00DEM050TO".
           02 FILLER               PIC X(11) VALUE "C00DEM051TO".
           02 FILLER               PIC X(11) VALUE "C00DEM052TO".
           02 FILLER               PIC X(11) VALUE "C00DEM053TO".
           02 FILLER               PIC X(11) VALUE "C00DEM054TO".
           02 FILLER               PIC X(11) VALUE "C00DEM055TO".
           02 FILLER               PIC X(11) VALUE "C00DEM056TO".
*
           02 FILLER               PIC X(11) VALUE "C00DEM057TO".
           02 FILLER               PIC X(11) VALUE "C00DEM058TO".
           02 FILLER               PIC X(11) VALUE "C00DEM059TO".
           02 FILLER               PIC X(11) VALUE "C00DEM060TO".
           02 FILLER               PIC X(11) VALUE "C00DEM061TO".
           02 FILLER               PIC X(11) VALUE "C00DEM062TO".
           02 FILLER               PIC X(11) VALUE "C00DEM063TO".
           02 FILLER               PIC X(11) VALUE "C00DEM064TO".
           02 FILLER               PIC X(11) VALUE "C00DEM065TO".
           02 FILLER               PIC X(11) VALUE "C00DEM066TO".
           02 FILLER               PIC X(11) VALUE "C00DEM067TO".
        01 M-NOMCAR-RECDES REDEFINES NOMCAR-RECDES.
           02 V-NOMCAR-RECDES      PIC X(11) OCCURS 34 TIMES.
        01 CAR-RECDES.
           02 02SCR013TO           PIC 9(03).
*
           02 00DEM035TO           PIC X(60).
           02 00DEM036TO           PIC X(02).
           02 00DEM037TO           PIC X(02).
           02 00DEM038TO           PIC X(20).
           02 00DEM039TO           PIC X(20).
           02 00DEM040TO           PIC 9(12).
           02 00DEM041TO           PIC X(60).
           02 00DEM042TO           PIC 9(12).
           02 00DEM043TO           PIC X(02).
           02 00DEM044TO           PIC X(20).
           02 00DEM045TO           PIC X(20).
*
           02 00DEM046TO           PIC X(60).
           02 00DEM047TO           PIC X(02).
           02 00DEM048TO           PIC X(02).
           02 00DEM049TO           PIC X(20).
           02 00DEM050TO           PIC X(20).
           02 00DEM051TO           PIC 9(12).
           02 00DEM052TO           PIC X(60).
           02 00DEM053TO           PIC 9(12).
           02 00DEM054TO           PIC X(02).
           02 00DEM055TO           PIC X(20).
           02 00DEM056TO           PIC X(20).
*
           02 00DEM057TO           PIC X(60).
           02 00DEM058TO           PIC X(02).
           02 00DEM059TO           PIC X(02).
           02 00DEM060TO           PIC X(20).
           02 00DEM061TO           PIC X(20).
           02 00DEM062TO           PIC 9(12).
           02 00DEM063TO           PIC X(60).
           02 00DEM064TO           PIC 9(12).
           02 00DEM065TO           PIC X(02).
           02 00DEM066TO           PIC X(20).
           02 00DEM067TO           PIC X(20).
*
