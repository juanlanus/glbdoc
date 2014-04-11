REMARK* BDIENDEU-WS.v.0001
      **********************************                                SUP.001 
      * INICIO VARIABLES ENDEUDAMIENTO:                                 SUP.001 
      **********************************                                SUP.001 
        77  W-MENSAJE-AUX           PIC X(3000).                        SUP.001 
        77  W-DTH-END               PIC 9 VALUE 0.                      ANO2000 
        01  W-INDICADORES.                                              SUP.001 
            02 W-NUM-TOT            PIC 9(1).                           SUP.001 
            02 W-NUM-LIN-TOT        PIC 9(1).                           SUP.001 
            02 W-FIN-ARCH           PIC 9(1).                           SUP.001 
            02 W-EXI-REG            PIC 9(1).                           SUP.001 
            02 W-ENCABEZA           PIC 9(1).                           SUP.001 
            02 W-INDICE             PIC 9(2).                           SUP.001 
            02 W-AUX                PIC 9(2).                           SUP.001 
            02 W-ENVIAR-TEXTO       PIC 9(1).                           SUP.009 
            02 W-MOSTRO-CTI         PIC 9(1).
                                                                        SUP.001 
        01  W-AUXILIARES.                                               SUP.001 
            02 W-NUM-CUENTAS        PIC 9(3).                           SUP.001 
            02 W-VALOR              PIC 9(17).                          SUP.001 
            02 W-VALOR-CTIN         PIC 9(17).                          SUP.007 
            02 W-VALOR-CTCP         PIC 9(17).                          SUP.008 
            02 W-COCIENTE           PIC 9(17).                          SUP.007 
            02 W-VALOR-TOT          PIC 9(17).                          SUP.001 
            02 W-TIP-CONS           PIC 9(1).                           SUP.001 
            02 W-CALIF              PIC 9(1).                           SUP.001 
            02 W-CANTIDAD           PIC 9(3).                           SUP.001 
            02 W-CANTIDAD-CTIN      PIC 9(3).                           SUP.007 
            02 W-CANTIDAD-CTCP      PIC 9(3).                           SUP.008 
                                                                        SUP.001 
        01  W-LINEAS-PANTALLA.                                          SUP.001 
            02 W-LINEA-1            PIC X(80) VALUE ALL "=".            SUP.001 
            02 W-LINEA-PAN          PIC X(80).                          SUP.001 
            02 W-LINEA-AUX          PIC X(80).                          SUP.007 
                                                                        SUP.001 
        01  W-FECHAS.                                                   SUP.001 
            02 W-MES-ANT            PIC 9(2).                           SUP.001 
            02 W-FECHA-AUX          PIC X(4).                           SUP.001 
            02 W-FECHA.                                                 SUP.001 
               03 W-ANIO            PIC 9(4).                           SUP.001 
               03 W-MES             PIC 9(2).                           SUP.001 
               03 W-DIA             PIC 9(2).                           SUP.001 
            02 W-FEC-ART-15-AUX     PIC 9(8).                           SUP.001 
            02 W-FEC-ART-15.                                            SUP.001 
               03 W-ART-15-ANIO            PIC 9(4).                    SUP.001 
               03 W-ART-15-MES             PIC 9(2).                    SUP.001 
               03 W-ART-15-DIA             PIC 9(2).                    SUP.001 
                                                                        SUP.001 
        01  EDITADAS.                                                   SUP.001 
            03 E-VALOR              PIC $$,$$$,$$$,$$9.                 SUP.001 
                                                                        SUP.001 
        01 W-TABLA-CALIF            VALUE "ABCDENR".                    SUP.001 
           02 W-NOM-CALIF           PIC X(1) OCCURS 7 TIMES.            SUP.001 
                                                                        SUP.001 
        01 W-TABLA-TOT-FECHA.                                           SUP.001 
           02 W-FECHA-TOT           OCCURS 5 TIMES.                     SUP.001 
               03 W-ANIO-TOT        PIC 9(4).                           SUP.001 
               03 W-MES-TOT         PIC 9(2).                           SUP.001 
               03 W-DIA-TOT         PIC 9(2).                           SUP.001 
                                                                        SUP.001 
        01 W-TABLA-TOT-ML.                                              SUP.001 
           02 W-CONSUMO-ML          OCCURS 5 TIMES.                     SUP.001 
              03 W-CAN-CONS-ML      PIC 9(3)   OCCURS 7 TIMES.          SUP.001 
              03 W-VAL-CONS-ML      PIC 9(11)  OCCURS 7 TIMES.          SUP.001 
           02 W-COMERCI-ML          OCCURS 5 TIMES.                     SUP.001 
              03 W-CAN-COME-ML      PIC 9(3)   OCCURS 7 TIMES.          SUP.001 
              03 W-VAL-COME-ML      PIC 9(11)  OCCURS 7 TIMES.          SUP.001 
           02 W-HIPOTEC-ML          OCCURS 5 TIMES.                     SUP.001 
              03 W-CAN-HIPO-ML      PIC 9(3)   OCCURS 7 TIMES.          SUP.001 
              03 W-VAL-HIPO-ML      PIC 9(11)  OCCURS 7 TIMES.          SUP.001 
                                                                        SUP.001 
        01 W-TABLA-TOT-ME.                                              SUP.001 
           02 W-CONSUMO-ME          OCCURS 5 TIMES.                     SUP.001 
              03 W-CAN-CONS-ME      PIC 9(3)   OCCURS 7 TIMES.          SUP.001 
              03 W-VAL-CONS-ME      PIC 9(11)  OCCURS 7 TIMES.          SUP.001 
           02 W-COMERCI-ME          OCCURS 5 TIMES.                     SUP.001 
              03 W-CAN-COME-ME      PIC 9(3)   OCCURS 7 TIMES.          SUP.001 
              03 W-VAL-COME-ME      PIC 9(11)  OCCURS 7 TIMES.          SUP.001 
           02 W-HIPOTEC-ME          OCCURS 5 TIMES.                     SUP.001 
              03 W-CAN-HIPO-ME      PIC 9(3)   OCCURS 7 TIMES.          SUP.001 
              03 W-VAL-HIPO-ME      PIC 9(11)  OCCURS 7 TIMES.          SUP.001 
                                                                        SUP.001 
        01 W-TABLA-TOT-CTIN-ML.                                         SUP.007 
           03 W-CAN-CTIN-CONS-ML     PIC 9(3)   OCCURS 7 TIMES.         SUP.007 
           03 W-VAL-CTIN-CONS-ML     PIC 9(11)  OCCURS 7 TIMES.         SUP.007 
           03 W-CAN-CTIN-COME-ML     PIC 9(3)   OCCURS 7 TIMES.         SUP.007 
           03 W-VAL-CTIN-COME-ML     PIC 9(11)  OCCURS 7 TIMES.         SUP.007 
           03 W-CAN-CTIN-HIPO-ML     PIC 9(3)   OCCURS 7 TIMES.         SUP.007 
           03 W-VAL-CTIN-HIPO-ML     PIC 9(11)  OCCURS 7 TIMES.         SUP.007 

        01 W-TABLA-TOT-CTIN-ME.                                         SUP.007 
           03 W-CAN-CTIN-CONS-ME     PIC 9(3)   OCCURS 7 TIMES.         SUP.007 
           03 W-VAL-CTIN-CONS-ME     PIC 9(11)  OCCURS 7 TIMES.         SUP.007 
           03 W-CAN-CTIN-COME-ME     PIC 9(3)   OCCURS 7 TIMES.         SUP.007 
           03 W-VAL-CTIN-COME-ME     PIC 9(11)  OCCURS 7 TIMES.         SUP.007 
           03 W-CAN-CTIN-HIPO-ME     PIC 9(3)   OCCURS 7 TIMES.         SUP.007 
           03 W-VAL-CTIN-HIPO-ME     PIC 9(11)  OCCURS 7 TIMES.         SUP.007 

        01 W-TABLA-TOT-CTCP.                                            SUP.008 
           03 W-CAN-CTCP-ML     PIC 9(3)   OCCURS 7 TIMES.              SUP.008 
           03 W-VAL-CTCP-ML     PIC 9(11)  OCCURS 7 TIMES.              SUP.008 
           03 W-CAN-CTCP-ME     PIC 9(3)   OCCURS 7 TIMES.              SUP.008 
           03 W-VAL-CTCP-ME     PIC 9(11)  OCCURS 7 TIMES.              SUP.008 
                                                                        SUP.001 
        77 X-S01-FECHA      PIC X(4).

        01 S01-REG-PLANO.                                               SUP.009 
           03 S01-00-TIPO-REG    PIC 9(2).                              SUP.009 
           03 S01-01-CALIFICA    PIC X(1).                              SUP.009 
           03 S01-02-VALOR       PIC 9(17).                             SUP.009 
           03 S01-03-TIP-DEUDA   PIC X(3).                              SUP.009 
           03 S01-04-MONEDA      PIC X(2).                              SUP.009 
           03 S01-05-CANTIDAD    PIC 9(3).                              SUP.009 
           02 S01-AREA-VIEJA.
              03 S01-06-FEC-CORTE   PIC 9(4).                           SUP.009 
              03 S01-07-ENTIDAD     PIC X(30).                          SUP.009 
              03 S01-08-GARANTIA    PIC X(6).                           SUP.009 
              03 FILLER             PIC X(2).                           ANO2000
        01 S01-AREA-NUEVA.                                              ANO2000
              03 S01-06-FEC-CORTE-N PIC 9(6).                           ANO2000 
              03 S01-07-ENTIDAD-N   PIC X(30).                          ANO2000 
              03 S01-08-GARANTIA-N  PIC X(6).                           ANO2000 
        01 S02-REG-PLANO.                                               SUP.009 
           03 S02-00-TIPO-REG       PIC 9(2).                           SUP.009 
           03 S02-01-ART-15         PIC 9(2).                           SUP.009 
           03 S02-02-FEC-ART-15     PIC X(8).                           SUP.009 
           03 S02-03-FEC-ACTUALIZ   PIC X(8).                           SUP.009 
           03 S02-04-NOM-RECLAMO    PIC X(45).                          SUP.009 
      ******************************                                    SUP.001 
      * FIN variables endeudamiento.                                    SUP.001 
      ******************************                                    SUP.001 
        COPY  BDIELINE.WS.
