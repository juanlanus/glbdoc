REMARK*CARACTBC-WS.0002
REMARK**********************************************************************
REMARK* RMB0002    * BCOP01: SE DEFINENEN LAS VARIABLES PROPIAS DEFINIDAS
REMARK* R.MARTINEZ *         PARA EL BANCO POPULAR PARA SER ACCEDIDAD POR
REMARK* 25-OCT-2009*         LOS PROGRAMAS DEL PRODUCTO DECISOR 
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PROPIAS PARA BANCO DE CREDITO
REMARK**********************************************************************
       01 VARIABLES-CARACTBC.
          02 02MOR001BC           PIC 9(3). *> MORAS  30 ULTIMOS 12 MESE
          02 02MOR002BC           PIC 9(3). *> MORAS  60 ULTIMOS 12 MESE
          02 02MOR003BC           PIC 9(3). *> MORAS  90 ULTIMOS 12 MESE
          02 02MOR004BC           PIC 9(3). *> MORAS 120 ULTIMOS 12 MESE
          02 02MOR005BC           PIC 9(3). *> MORAS  30 FIN ULT 12 MESE
          02 02MOR006BC           PIC 9(3). *> MORAS  30 REAL ULT 12 MESE
          02 02MOR007BC           PIC 9(3). *> MESE MORA 60 MAS RECIENTE 
BCOP01******************************************************************RMB0002
BCOP01*DEFINICION DE CARACTERISTICAS PROPIAS PARA BANCO POPULAR         RMB0002
BCOP01******************************************************************RMB0002
BCOP01 01 NOM-POR-CARACT PIC X(18) VALUE "ROINVEHPOTCCCOAHCT".          RMB0002
BCOP01 01 FILLER REDEFINES NOM-POR-CARACT.                              RMB0002
BCOP01    02 V-POR-CARACT   OCCURS 9 TIMES PIC X(02).                   RMB0002
BCOP01 01 VARIABLES-AUXCARACTBP.                                        RMB0002
BCOP01    02 SW-02MOR            PIC 9(1).                              RMB0002
BCOP01    02 DEBUGC              PIC 9(1) .                             RMB0002
BCOP01 01 VARIABLES-CARACTBP.                                           RMB0002
BCOP01    02 VAR-CARACTBP.                                              RMB0002
BCOP01       03 TABLA-PORTA-0001MOR OCCURS 9 TIMES.                     RMB0002
BCOP01         05 01MOR109       PIC 9(4). *> MORAS  30 ULTIMOS 12 MESE RMB0002
BCOP01         05 01MOR110       PIC 9(4). *> MORAS  60 ULTIMOS 12 MESE RMB0002
BCOP01         05 01MOR111       PIC 9(4). *> MORAS  90 ULTIMOS 12 MESE RMB0002
BCOP01         05 01MOR112       PIC 9(4). *> MORAS 120 ULTIMOS 12 MESE RMB0002
BCOP01    02 VAR-CARACTBPX REDEFINES VAR-CARACTBP.                      RMB0002
BCOP01*                                                                 RMB0002
BCOP01       05 01MOR109RO       PIC 9(4).                              RMB0002
BCOP01       05 01MOR110RO       PIC 9(4).                              RMB0002
BCOP01       05 01MOR111RO       PIC 9(4).                              RMB0002
BCOP01       05 01MOR112RO       PIC 9(4).                              RMB0002
BCOP01*                                                                 RMB0002
BCOP01       05 01MOR109IN       PIC 9(4).                              RMB0002
BCOP01       05 01MOR110IN       PIC 9(4).                              RMB0002
BCOP01       05 01MOR111IN       PIC 9(4).                              RMB0002
BCOP01       05 01MOR112IN       PIC 9(4).                              RMB0002
BCOP01*                                                                 RMB0002
BCOP01       05 01MOR109VE       PIC 9(4).                              RMB0002
BCOP01       05 01MOR110VE       PIC 9(4).                              RMB0002
BCOP01       05 01MOR111VE       PIC 9(4).                              RMB0002
BCOP01       05 01MOR112VE       PIC 9(4).                              RMB0002
BCOP01*                                                                 RMB0002
BCOP01       05 01MOR109HP       PIC 9(4).                              RMB0002
BCOP01       05 01MOR110HP       PIC 9(4).                              RMB0002
BCOP01       05 01MOR111HP       PIC 9(4).                              RMB0002
BCOP01       05 01MOR112HP       PIC 9(4).                              RMB0002
BCOP01*                                                                 RMB0002
BCOP01       05 01MOR109OT       PIC 9(4).                              RMB0002
BCOP01       05 01MOR110OT       PIC 9(4).                              RMB0002
BCOP01       05 01MOR111OT       PIC 9(4).                              RMB0002
BCOP01       05 01MOR112OT       PIC 9(4).                              RMB0002
BCOP01*                                                                 RMB0002
BCOP01       05 01MOR109CC       PIC 9(4).                              RMB0002
BCOP01       05 01MOR110CC       PIC 9(4).                              RMB0002
BCOP01       05 01MOR111CC       PIC 9(4).                              RMB0002
BCOP01       05 01MOR112CC       PIC 9(4).                              RMB0002
BCOP01*                                                                 RMB0002
BCOP01       05 01MOR109CO       PIC 9(4).                              RMB0002
BCOP01       05 01MOR110CO       PIC 9(4).                              RMB0002
BCOP01       05 01MOR111CO       PIC 9(4).                              RMB0002
BCOP01       05 01MOR112CO       PIC 9(4).                              RMB0002
BCOP01*                                                                 RMB0002
BCOP01       05 01MOR109AH       PIC 9(4).                              RMB0002
BCOP01       05 01MOR110AH       PIC 9(4).                              RMB0002
BCOP01       05 01MOR111AH       PIC 9(4).                              RMB0002
BCOP01       05 01MOR112AH       PIC 9(4).                              RMB0002
BCOP01*                                                                 RMB0002
BCOP01       05 01MOR109CT       PIC 9(4).                              RMB0002
BCOP01       05 01MOR110CT       PIC 9(4).                              RMB0002
BCOP01       05 01MOR111CT       PIC 9(4).                              RMB0002
BCOP01       05 01MOR112CT       PIC 9(4).                              RMB0002
BCOP01    02 VAR-00-02MORXXX.                                           RMB0002
BCOP01       05 00MOR109         PIC 9(3). *> MORAS  30 ULTIMOS 12 MESE RMB0002
BCOP01       05 00MOR110         PIC 9(3). *> MORAS  60 ULTIMOS 12 MESE RMB0002
BCOP01       05 00MOR111         PIC 9(3). *> MORAS  90 ULTIMOS 12 MESE RMB0002
BCOP01       05 00MOR112         PIC 9(3). *> MORAS 120 ULTIMOS 12 MESE RMB0002
BCOP01       05 02MOR002TO       PIC 9(3). *> PRODUCTOS EN MORA  30     RMB0002
BCOP01       05 02MOR003TO       PIC 9(3). *> PRODUCTSO EN MORA  60     RMB0002
BCOP01       05 02MOR004TO       PIC 9(3). *> PRODUCTSO EN MORA  90     RMB0002
BCOP01       05 02MOR005TO       PIC 9(3). *> PRODUCTSO EN MORA  120    RMB0002
BCOP01       05 02MOR006TO       PIC 9(3). *> PRODUCTOS CASITIGADOS     RMB0002
BCOP01       05 02MOR007TO       PIC 9(3). *> PRODUCTOS RECUPERADOS     RMB0002
BCOP01       05 02MOR109TO       PIC 9(3). *> MORAS  30 DIAS ULT 12 MESERMB0002
BCOP01       05 02MOR110TO       PIC 9(3). *> MORAS  60 DIAS ULT 12 MESERMB0002
BCOP01       05 02MOR111TO       PIC 9(3). *> MORAS  90 DIAS ULT 12 MESERMB0002
BCOP01       05 02MOR112TO       PIC 9(3). *> MORAS 120 DIAS ULT 12 MESERMB0002
BCOP01    02 IND-CARACTBP.                                              RMB0002
BCOP01       05 02MORI002TO             PIC 9(1).                       RMB0002
BCOP01       05 02MORI003TO             PIC 9(1).                       RMB0002
BCOP01       05 02MORI004TO             PIC 9(1).                       RMB0002
BCOP01       05 02MORI005TO             PIC 9(1).                       RMB0002
BCOP01       05 02MORI006TO             PIC 9(1).                       RMB0002
BCOP01       05 02MORI007TO             PIC 9(1).                       RMB0002
BCOP01       05 02MORI109TO             PIC 9(1).                       RMB0002
BCOP01       05 02MORI110TO             PIC 9(1).                       RMB0002
BCOP01       05 02MORI111TO             PIC 9(1).                       RMB0002
BCOP01       05 02MORI112TO             PIC 9(1).                       RMB0002
