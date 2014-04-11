REMARK*CARACTPER-016.WS.v.0002.
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA CITIBANK       
REMARK**********************************************************************
REMARK* IMS0002    | CQ7065 - SE AGREGAN NUEVAS CARACTERISTICAS        *
REMARK* SEP.13.2012|                                                   *
REMARK* M.SALAMANCA|                                                   *
REMARK******************************************************************

        01 VARIABLES-PER-016.
           03 ROFLAG-016                PIC 9(03)  VALUE ZEROS.
           03 ROFLAG-016-C              PIC 9(03)  VALUE ZEROS.
           03 ROOFLAG-016               PIC 9(03)  VALUE ZEROS.
           03 RODFLAG-016               PIC 9(03)  VALUE ZEROS.
           03 RODFLAG-016-NUM           PIC 9(03)  VALUE ZEROS.
           03 RODFLAG-016-MOR           PIC 9(03)  VALUE ZEROS.
           03 RODFLAG-016-C             PIC 9(03)  VALUE ZEROS.
           03 ROGFLAG-016               PIC 9(03)  VALUE ZEROS.
           03 ROVFLAG-016               PIC 9(03)  VALUE ZEROS.
           03 ROVFLAG-016-NUM           PIC 9(03)  VALUE ZEROS.
           03 ROVFLAG-016-MOR           PIC 9(03)  VALUE ZEROS.
           03 ROVFLAG-016-3             PIC 9(03)  VALUE ZEROS.
           03 ROVFLAG-016-6             PIC 9(03)  VALUE ZEROS.
           03 ROVFLAG-016-12            PIC 9(03)  VALUE ZEROS.
           03 ROVFLAG-016-C             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-016-0             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-016-1             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-016-2             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-016-3             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-016-4             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-016-6             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-016-7             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-016-N             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-3M-1              PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-3M-2              PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-3M-3              PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-3M-4              PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-6M-1              PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-6M-2              PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-6M-3              PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-6M-4              PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-12M-1             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-12M-2             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-12M-3             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-12M-4             PIC 9(03)  VALUE ZEROS.
           03 ROVALUE-A                 PIC 9(04)  VALUE ZEROS.
           03 ROVALUE-B                 PIC 9(04)  VALUE ZEROS.
           03 ROVALUE-C                 PIC 9(04)  VALUE ZEROS.
           03 ROVALUE-D                 PIC 9(04)  VALUE ZEROS.
           03 ROVALUE-E                 PIC 9(04)  VALUE ZEROS.
           03 MXFLAG-016                PIC 9(03)  VALUE ZEROS.
           03 MXFLAG-016-C              PIC 9(03)  VALUE ZEROS.
           03 MXOFLAG-016               PIC 9(03)  VALUE ZEROS.
           03 MXDFLAG-016               PIC 9(03)  VALUE ZEROS.
           03 MXDFLAG-016-NUM           PIC 9(03)  VALUE ZEROS.
           03 MXDFLAG-016-MOR           PIC 9(03)  VALUE ZEROS.
           03 MXDFLAG-016-C             PIC 9(03)  VALUE ZEROS.
           03 MXGFLAG-016               PIC 9(03)  VALUE ZEROS.
           03 MXVFLAG-016               PIC 9(03)  VALUE ZEROS.
           03 MXVFLAG-016-NUM           PIC 9(03)  VALUE ZEROS.
           03 MXVFLAG-016-MOR           PIC 9(03)  VALUE ZEROS.
           03 MXVFLAG-016-3             PIC 9(03)  VALUE ZEROS.
           03 MXVFLAG-016-6             PIC 9(03)  VALUE ZEROS.
           03 MXVFLAG-016-12            PIC 9(03)  VALUE ZEROS.
           03 MXVFLAG-016-C             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-016-0             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-016-1             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-016-2             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-016-3             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-016-4             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-016-6             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-016-7             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-016-N             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-3M-1              PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-3M-2              PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-3M-3              PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-3M-4              PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-6M-1              PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-6M-2              PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-6M-3              PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-6M-4              PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-12M-1             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-12M-2             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-12M-3             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-12M-4             PIC 9(03)  VALUE ZEROS.
           03 MXVALUE-A                 PIC 9(04)  VALUE ZEROS.
           03 MXVALUE-B                 PIC 9(04)  VALUE ZEROS.
           03 MXVALUE-C                 PIC 9(04)  VALUE ZEROS.
           03 MXVALUE-D                 PIC 9(04)  VALUE ZEROS.
           03 MXVALUE-E                 PIC 9(04)  VALUE ZEROS.
CQ7065*                                                                 IMS0002
CQ7065     03 RO-01FLAG-016-N           PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FLAG-016-J           PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01OPFLAG-016-N         PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01OPFLAG-016-NE        PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01OPFLAG-016-J         PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01DFLAG-016-N          PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01DFLAG-016-N-END      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01DFLAG-016-J-END      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01VFLAG-016-N-END      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01VFLAG-016-J-END      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FFLAG-016-N          PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FFLAG-016-INI-N      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FFLAG-016-SDO-N      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FFLAG-016-CUO-N      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FFLAG-016-J          PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FFLAG-016-INI-J      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FFLAG-016-SDO-J      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FFLAG-016-CUO-J      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 RO-01FFLAG-016-991-N      PIC 9(03)  VALUE ZEROS.         IMS0002
CQ7065     03 UTILIZ-JURISC             PIC 9(03)V9(04) VALUE ZEROS.    IMS0002
CQ5220     03 I-IND-JUR                 PIC 9(03)  VALUE ZEROS.
CQ5220     03 W-SUSC-JURI.
CQ5220        05 W-SUS-JURI-T           PIC 9(01)  VALUE ZEROS.
CQ5220        05 W-SUS-JURI-S           PIC 9(06)  VALUE ZEROS.
CQ7065*                                                                 IMS0002
CQ7065  01 TABLA-SUSC-JURI.                                             IMS0002
CQ7065     03 TB-SUSC-JURI OCCURS 40 ASCENDING KEY IS SUS-JURI          IMS0002
CQ7065                                INDEXED BY I-JUR.                 IMS0002
CQ7065        05 SUS-JURI               PIC 9(06).                      IMS0002
CQ7065 01  ENT-JURISCOOP                PIC X(02)  VALUE SPACES.        IMS0002
CQ7065 01  SW-SUSCR-JURI                PIC 9(01)  VALUE ZEROS.         IMS0002

        01 PORTAF-PER-016.   
           03 COD-CTA-PORTAF-016       PIC X(05)   VALUE SPACES.

        01 PERSONALIZACION-16.
           02 PER-16-NIVEL-01-MOR.
              03 01MOR999XX016.
                 04 01MOR999RO016       PIC X(03).
                 04 01MOR999MX016       PIC X(03).
              03 01MOR998XX016.
                 04 01MOR998RO016       PIC X(03).
                 04 01MOR998MX016       PIC X(03).
              03 01MOR997XX016.
                 04 01MOR997RO016       PIC X(03).
                 04 01MOR997MX016       PIC X(03).
              03 01MOR996XX016.
                 04 01MOR996RO016       PIC X(03).
                 04 01MOR996MX016       PIC X(03).
              03 01MOR995XX016.
                 04 01MOR995RO016       PIC X(03).
                 04 01MOR995MX016       PIC X(03).
              03 01MOR994XX016.
                 04 01MOR994RO016       PIC X(03).
                 04 01MOR994MX016       PIC X(03).
              03 01MOR993XX016.
                 04 01MOR993RO016       PIC X(03).
                 04 01MOR993MX016       PIC X(03).
              03 01MOR992XX016.
                 04 01MOR992RO016       PIC X(03).
                 04 01MOR992MX016       PIC X(03).
              03 01MOR991XX016.
                 04 01MOR991RO016       PIC X(03).
                 04 01MOR991MX016       PIC X(03).
              03 01MOR990XX016.
                 04 01MOR990RO016       PIC X(03).
                 04 01MOR990MX016       PIC X(03).
              03 01MOR989XX016.
                 04 01MOR989RO016       PIC X(03).
                 04 01MOR989MX016       PIC X(03).
              03 01MOR988XX016.
                 04 01MOR988RO016       PIC X(03).
                 04 01MOR988MX016       PIC X(03).
              03 01MOR987XX016.
                 04 01MOR987RO016       PIC X(03).
                 04 01MOR987MX016       PIC X(03).
              03 01MOR986XX016.
                 04 01MOR986RO016       PIC X(03).
                 04 01MOR986MX016       PIC X(03).
              03 01MOR985XX016.
                 04 01MOR985RO016       PIC X(03).
                 04 01MOR985MX016       PIC X(03).
              03 01MOR984XX016.
                 04 01MOR984RO016       PIC X(03).
                 04 01MOR984MX016       PIC X(03).
              03 01MOR983XX016.
                 04 01MOR983RO016       PIC X(03).
                 04 01MOR983MX016       PIC X(03).
              03 01MOR982XX016.
                 04 01MOR982RO016       PIC X(03).
                 04 01MOR982MX016       PIC X(03).
              03 01MOR981XX016.
                 04 01MOR981RO016       PIC X(03).
                 04 01MOR981MX016       PIC X(03).

           02 PER-16-NIVEL-01-NUM.
              03 01NUM999XX016.
                 04 01NUM999RO016       PIC X(04).  
                 04 01NUM999MX016       PIC X(04).  
              03 01NUM998XX016.
                 04 01NUM998RO016       PIC X(04).  
                 04 01NUM998MX016       PIC X(04).  
              03 01NUM997XX016.
                 04 01NUM997RO016       PIC X(04).  
                 04 01NUM997MX016       PIC X(04).  
              03 01NUM996XX016.
                 04 01NUM996RO016       PIC X(04).  
                 04 01NUM996MX016       PIC X(04).  
              03 01NUM995XX016.
                 04 01NUM995RO016       PIC X(04).  
                 04 01NUM995MX016       PIC X(04).  
CQ7065*                                                                 IMS0002
CQ7065        03 01NUM994XX016.                                         IMS0002
CQ7065           04 01NUM994RO016       PIC 9(03).                      IMS0002
CQ7065        03 01NUM993XX016.                                         IMS0002
CQ7065           04 01NUM993RO016       PIC 9(03).                      IMS0002
CQ7065        03 01NUM992XX016.                                         IMS0002
CQ7065           04 01NUM992RO016       PIC 9(03).                      IMS0002
CQ7065*                                                                 IMS0002

           02 PER-16-NIVEL-01-ACP.
              03 01ACP999XX016.
                 04 01ACP999RO016       PIC X(03).  
                 04 01ACP999MX016       PIC X(03).  

CQ7065*                                                                 IMS0002
CQ7065     02 PER-16-NIVEL-01-END.                                      IMS0002
CQ7065        03 01END999XX016.                                         IMS0002
CQ7065           04 01END999RO016       PIC 9(11).                      IMS0002
CQ7065        03 01END998XX016.                                         IMS0002
CQ7065           04 01END998RO016       PIC 9(11).                      IMS0002
CQ7065        03 01END997XX016.                                         IMS0002
CQ7065           04 01END997RO016       PIC 9(11).                      IMS0002
CQ7065        03 01END996XX016.                                         IMS0002
CQ7065           04 01END996RO016       PIC 9(11).                      IMS0002
CQ7065        03 01END995XX016.                                         IMS0002
CQ7065           04 01END995RO016       PIC 9(11).                      IMS0002
CQ7065        03 01END994XX016.                                         IMS0002
CQ7065           04 01END994RO016       PIC 9(11).                      IMS0002
CQ7065        03 01END993XX016.                                         IMS0002
CQ7065           04 01END993RO016       PIC 9(11).                      IMS0002
CQ7065        03 01END992XX016.                                         IMS0002
CQ7065           04 01END992RO016       PIC 9(11).                      IMS0002
CQ7065        03 01END991XX016.                                         IMS0002
CQ7065           04 01END991RO016       PIC 9(03).                      IMS0002
