REMARK*** SCODRU-WS.v.0008
REMARK**********************************************************************
REMARK*** DEFINICION DE VARIABLES PARA UTILIZAR EL SCORING COMO LIBRERIA ***
REMARK**********************************************************************
REMARK*VHF0002    *SE ADICIONA COMO PARAMETRO LA ALTURA TOTAL DE MORA  *
REMARK*VICTOR HUGO*                                                    *
REMARK*FORERO     *                                                    *
REMARK*JUL.11/2000*                                                    *
REMARK****************************************************************** 
REMARK*VHF0003    * RAZDUP. SE DEFINE TABLA PARA CONTROLAR DESPLIEGUE  *
REMARK*VICTOR HUGO* DE RAZONES DUPLICADAS                              *
REMARK*FORERO     *                                                    *
REMARK*FEB.13/2001*                                                    *
REMARK****************************************************************** 
REMARK*VHF0004    * VALCON - SE APLIAN OCURRENCIAS PARA VECTORES DE    *
REMARK*VICTOR HUGO*          CUENTAS VIGENTES Y CONSULTAS REALIZADAS   *
REMARK*FORERO     *                                                    *
REMARK*MAR.09/2001*                                                    *
REMARK******************************************************************
REMARK*VHF0005    * TIPO6  - SE INCLUYE COMO PARAMETRO EL PRIVILEGIO DE*
REMARK*VICTOR HUGO*          VALIDAR APELLIDO DE LA CLAVE PARA USARLO  *
REMARK*FORERO     *          EN LA VALIDACION MENCIONADA.              *
REMARK*JUL.10/2001*                                                    *
REMARK******************************************************************
REMARK*VHF0006    * SE ADICIONAN VARIABLES PARA MANEJO DE PARAMETRO QUE*
REMARK*VICTOR HUGO* INDICA SI LOS ADJETIVOS DE CARTERA ESTAN CORREGIDOS*
REMARK*FORERO     * O SI VIENEN TRASTOCADOS.                           *
REMARK*JUL.10/2001*                                                    *
REMARK******************************************************************
REMARK* RMB0007     * CLAS01 - SE IMPLEMENTA LA OPCION DE CLASIFICACION*
REMARK* R.MARTINEZ  *          PARA LOS SCORES GENERICO.               *
REMARK* MAR.20/2003 *                                                  *
REMARK******************************************************************
REMARK*VHF0008    * ENDEUD - SE DEFINE UNA NUEVA VARIABLE QUE PERMITE  *
REMARK*VICTOR HUGO*          DETERMINAR EL PRIMER REGISTRO DE          *
REMARK*FORERO     *          ENDEUDAMIENTO.                            *
REMARK*MAY.20/2005*                                                    *
REMARK******************************************************************

REMARK*** TABLAS
REMARK*** ======

REMARK*** SE MANEJA UNA GRAN TABLA QUE CONTIENE EL ESQUEMA GENERAL DE
REMARK*** PARAMETROS DE EVALUACION, CONFORMADO POR LAS DIFERENTES
REMARK*** SUBTABLAS. ESTA TABLA ES BIDIMENSIONAL, DONDE EL PRIMER INDICE
REMARK*** CORRESPONDE A LA SUBTABLA ASOCIADA Y EL SEGUNDO A LA
REMARK*** CORRESPODIENTE EN LA SECCION EN ANALISIS. SE UTILIZAN TRES
REMARK*** GRUPOS DE TABLAS: TABLA TIPOS EVALUACION, TABLAS TIPO CUENTA Y
REMARK*** TABLAS DE CALIFICACION
       01 WS-ESQEVA-SCODRU.
          03 WS-TABEVA-SCODRU            OCCURS 20  TIMES.
             05 WS-REGGEN-SCODRU.
                07 FILLER                PIC X(65)  OCCURS 99 TIMES.
REMARK********* GRUPO TABLA TIPO DE EVALUACION
             05 WS-TABTEV-SCODRU         REDEFINES WS-REGGEN-SCODRU.
                07 WS-INDTEV-SCODRU      OCCURS 99  TIMES.
                   09 WS-IDEEVA-SCODRU   PIC X(01).
                   09 WS-DESEVA-SCODRU   PIC X(10).
                   09 FILLER             PIC X(06).
                   09 WS-MINEVA-SCODRU   PIC 9(10).
                   09 WS-DEFEVA-SCODRU   PIC 9(03).
REMARK*************** EL SIGUIENTE CAMPO LLEVA EL CONTEO DE EVALUACION POR
REMARK*************** CIUDADANO PARA COMPARAR CONTRA EL "AL MENOS" DE LA TABLA
REMARK*************** DE EVALUACION FINAL. DEBE INICIALIZARCE CADA VEZ QUE SE
REMARK*************** ANALICE UN NUEVO CIUDADANO
                   09 WS-ACUEVA-SCODRU   PIC 9(13).
                   09 FILLER             PIC X(22).
REMARK********* GRUPO TABLAS TIPO CUENTA
             05 WS-TABTCT-SCODRU         REDEFINES   WS-REGGEN-SCODRU.
                07 WS-INDTCT-SCODRU      OCCURS 99   TIMES.
                   09 FILLER             PIC X(14).
                   09 WS-PESCTA-SCODRU   PIC 9(03).
                   09 WS-MINCTA-SCODRU   PIC 9(10).
                   09 WS-PSNCTA-SCODRU   PIC 9(03).
REMARK*************** CAMPOS PARA LLEVAR EL ACUMULADO DE OCURRENCIAS O
REMARK*************** VALORES, LA PEOR EVALUACION, EL CONTEO DE CUENTAS
REMARK*************** EL ACUMULADO DE CALIFICACIONES, (PARA CUENTAS
REMARK*************** HIPOTECARIAS, COMERCIALES O DE CONSUMO) Y EL NUMERO
REMARK*************** DE CUENTAS POR CIUDADANO PARA CADA TIPO DE CARTERA.
REMARK*************** DEBERAN INICIALIZARCE PARA CADA CIUDADANO.

                   09 WS-ESTCTA-SCODRU.
                      11 WS-ACUCTA-SCODRU PIC 9(13).
                      11 WS-MEVCTA-SCODRU PIC 9(01).
                      11 WS-NUMCTA-SCODRU PIC 9(06).
                      11 WS-CALCTA-SCODRU PIC S9(13)V99.
REMARK********* GRUPO TABLAS CALIFICACION
             05 WS-TABTCA-SCODRU          REDEFINES  WS-REGGEN-SCODRU.
                07 WS-INDTCA-SCODRU       OCCURS 99  TIMES.
                   09 FILLER              PIC X(10).
                   09 WS-NOTCAL-SCODRU    PIC S9(02)V9(02).
                   09 WS-EVACAL-SCODRU    PIC 9(03).
                   09 WS-MAXCAL-SCODRU    PIC 9(10).
                   09 WS-ESNCAL-SCODRU    PIC 9(03).
REMARK*************** EL SIGUIENTE CAMPO LLEVA EL ACUMULADO POR OCURRENCIAS
REMARK*************** O VALOR DE ESTADOS POR CIUDADANO PARA COMPARAR CONTRA
REMARK*************** EL "MAXIMO" EN LA TABLA DE CALIFICACIONES DEBE
REMARK*************** INICIALIZARCE CADA VEZ QUE SE ANALICE UN NUEVO CIUDADANO
                   09 WS-ACUCAL-SCODRU    PIC 9(13).
                   09 FILLER              PIC X(22).
REMARK********* GRUPO ESTADOS (INDICES PARA ESTADOS ACTUALES E HISTORICOS)
             05 WS-TABIES-SCODRU          REDEFINES  WS-REGGEN-SCODRU.
                07 WS-INDIES-SCODRU       OCCURS 99  TIMES.
                   09 FILLER              PIC X(14).
                   09 WS-ACTEST-SCODRU    PIC 9(03).
                   09 FILLER              PIC X(10).
                   09 WS-HISEST-SCODRU    PIC 9(03).
                   09 FILLER              PIC X(35).
CLAS01       05 WS-TABREC-SCODRU         REDEFINES WS-REGGEN-SCODRU.    RMB0007 
CLAS01          07 WS-INDREC-SCODRU      OCCURS 99  TIMES.              RMB0007 
CLAS01             09 WS-HAYCLA-SCODRU    PIC X(2).                     RMB0007 
CLAS01             09 WS-RANINI-SCODRU    PIC S9(3)V9(2).               RMB0007 
CLAS01             09 WS-RANFIN-SCODRU    PIC S9(3)V9(2).               RMB0007 
CLAS01             09 WS-CLASIF-SCODRU    PIC 9(3).                     RMB0007 
CLAS01             09 WS-RECLAS-SCODRU    PIC 9(3).                     RMB0007 
CLAS01             09 WS-RECRAZ-SCODRU    PIC X(4).                     RMB0007 
CLAS01             09 FILLER              PIC X(43).                    RMB0007 

REMARK*** TABLA DE DESCRIPCION DE CODIGOS DE RETORNOS. EL SUBINDICE
REMARK*** CORRESPONDE AL CODIGO DE RETORNO
       01 WS-TABRET-SCODRU.
          03 FILLER                       PIC X(43)  VALUE
             "ERR: SIN PARAMETROS GENERALES DE SUSCRIPTOR".
          03 FILLER                       PIC X(43)  VALUE
             "ERR: SIN TABLAS DE PARAMETROS PARA SUSCRIP.".
          03 FILLER                       PIC X(43)  VALUE
             "ERR: EN PARAMETRO TIPO TABLA X PERSONA     ".
          03 FILLER                       PIC X(43)  VALUE
             "ERR: CODIGO DE SUBTABLA INVALIDO           ".
          03 FILLER                       PIC X(43)  VALUE
             "ERR: INDICE EN SUBTABLA INVALIDO           ".
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
          03 FILLER                       PIC X(43)  VALUE  SPACES.
       01 WS-TABRET-R-SCODRU              REDEFINES  WS-TABRET-SCODRU.
          03 WS-DESERR-SCODRU             PIC X(43)  OCCURS 20 TIMES.

REMARK*** TABLA DE TIPO DE PERSONA DE ACUERDO AL TIPO DE IDENTIDAD:
REMARK*** 0-NO DEFINIDA   1-NATURAL    2-JURIDICA
       01 WS-TABTPE-SCODRU                PIC X(09)  VALUE "122100000".
       01 WS-TABPER-SCODRU                PIC 9(01)  OCCURS 9 TIMES
          REDEFINES WS-TABTPE-SCODRU.

REMARK*** TABLA DE TIPOS DE TABLA (TIPO-CUENTA,CALIFICACION,...)
       01 WS-TABTTA-SCODRU.
          03 WS-TIPTAB-SCODRU             PIC 9(01)  OCCURS 20 TIMES.

REMARK*** TABLA DE INDICES PARA MORA MAXIMA
       01 WS-TABMMX-SCODRU                PIC X(18)  VALUE
          "161718190000000000".
       01 WS-TABMMX-R-SCODRU              REDEFINES  WS-TABMMX-SCODRU.
          03 WS-TABESM-SCODRU             PIC 9(02)  OCCURS 9 TIMES.

REMARK*** TABLA PARA CONTADORES DE RAZON SEGUN EVALUACION
       01 WS-VCORAZ-SCODRU.
          03 WS-CONRAZ-SCODRU             PIC 9(02)  OCCURS 9 TIMES.

REMARK*** TABLA DE RAZONES 10 POR CADA TIPO EVALUACION
       01 WS-TABRXE-SCODRU.
          03 WS-TABREV-SCODRU             OCCURS 9  TIMES.
             05 WS-CODRAZ-SCODRU          OCCURS 10 TIMES.
                07 WS-TABRAZ-SCODRU       PIC 9(02).
                07 WS-INDRAZ-SCODRU       PIC 9(02).

REMARK*** TABLA PARA VERIFICACION DE RAZONES DUPLICADAS                 VHF0003
RAZDUP 01 WS-TABDRZ-SCODRU.                                             VHF0003
RAZDUP    03 WS-TABDUP-SCODRU             OCCURS 90 TIMES.              VHF0003
RAZDUP       05 WS-TABRDU-SCODRU          PIC 9(02).                    VHF0003
RAZDUP       05 WS-INDRDU-SCODRU          PIC 9(02).                    VHF0003

REMARK*** TABLA DE CUENTAS APROBADAS. CONTIENE SOLO CUENTAS VIGENTES
REMARK*** TANTO DE CARTERA COMO DE CUENTAS CORRIENTES Y SECTOR REAL
       01 WS-TABCAP-SCODRU.
VALCON    03 WS-TBCTAP-SCODRU             OCCURS 10000 TIMES.           VHF0004
VALCON       05 WS-FECAPR-SCODRU          PIC 9(06) COMP-3.             VHF0004
VALCON       05 WS-NSUAPR-SCODRU          PIC 9(11) COMP-3.             VHF0004
VALCON       05 WS-TCTAPR-SCODRU          PIC 9(02) COMP-3.             VHF0004
VALCON       05 WS-INDAPR-SCODRU          PIC 9(02) COMP-3.             VHF0004

REMARK*** TABLA DE CONSULTAS REALIZADAS AL CIUDADANO.
       01 WS-TABCONSUL-SCODRU.
VALCON    03 WS-TABCON-SCODRU             OCCURS 10000 TIMES.           VHF0004
VALCON       05 WS-CSUCON-SCODRU          PIC 9(06) COMP-3.             VHF0004
VALCON       05 WS-TCTCON-SCODRU          PIC 9(02) COMP-3.             VHF0004

REMARK*** TABLA DE INDICES PARA RESULTADOS DE CONSULTA
       01 WS-TNDRESCON-SCODRU             PIC X(20) VALUE
          "00000000080706000000".
       01 WS-TNDRESCON-R-SCODRU           REDEFINES
          WS-TNDRESCON-SCODRU.
          03 WS-INDRCO-SCODRU             PIC 9(02) OCCURS 10 TIMES.

REMARK*** VARIABLES GENERALES
REMARK*** ===================
CLAS01 01 WS-NEWCLA-SCODRU                PIC 9(02) VALUE ZEROS.        RMB0007 
CLAS01 01 WS-NEWRAZ-SCODRU                PIC X(04) VALUE SPACES.       RMB0007 
       01 WS-EVAL-SCODRU                  PIC 9(01) VALUE ZEROS.
       01 WS-NUM-A-PROM-SCODRU            PIC 9(02) VALUE ZEROS.
       01 WS-IND-ADJETIVO-SCODRU          PIC 9(01) VALUE ZEROS.
       01 WS-MMX-O-EMBAR-SCODRU           PIC 9(02) VALUE ZEROS.
       01 WS-IND-MULT-EVAL-SCODRU         PIC 9(01) VALUE ZEROS.
       01 WS-EVAL-MAYOR-SCODRU            PIC 9(01) VALUE ZEROS.
       01 WS-CALIF-DE-MAYOR-SCODRU        PIC S9(02)V99   VALUE ZEROS.
       01 WS-EVAL-ACTUAL-SCODRU           PIC 9(01) VALUE ZEROS.
       01 WS-I-SCODRU                     PIC 9(06) VALUE ZEROS.
       01 WS-J-SCODRU                     PIC 9(06) VALUE ZEROS.
       01 WS-K-SCODRU                     PIC 9(06) VALUE ZEROS.
       01 WS-L-SCODRU                     PIC 9(06) VALUE ZEROS.        VHF0003
       01 WS-M-SCODRU                     PIC 9(06) VALUE ZEROS.        VHF0003
       01 WS-IND-CONSULTA-SCODRU          PIC 9(01) VALUE ZEROS.
       01 WS-IND-TMP-SCODRU               PIC 9(01) VALUE ZEROS.
       01 WS-ENCPAR-SCODRU                PIC X(45) VALUE SPACES.
       01 WS-NOMPAR-SCODRU                PIC X(45) VALUE SPACES.
       01 WS-VALPAR-SCODRU                PIC X(45) VALUE SPACES.
       01 WS-LONENC-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-LONNOM-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-TABCAL-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-TABCTA-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-TABEST-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-INDCAL-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-INDCTA-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-INDEST-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-TABCAL-TMP-SCODRU            PIC 9(02) VALUE ZEROS.
       01 WS-INDCAL-TMP-SCODRU            PIC 9(02) VALUE ZEROS.
       01 WS-CTAVAL-TMP-SCODRU            PIC 9(06) VALUE ZEROS.
       01 WS-ACUCAL-TMP-SCODRU            PIC 9(13) VALUE ZEROS.
       01 WS-ACUCTA-TMP-SCODRU            PIC 9(13) VALUE ZEROS.
       01 WS-INDRAZ-TMP-SCODRU            PIC 9(02) VALUE ZEROS.
       01 WS-TABRAZ-TMP-SCODRU            PIC 9(02) VALUE ZEROS.
VALCON 01 WS-CTAAPR-SCODRU                PIC 9(05) VALUE ZEROS.        VHF0004
VALCON 01 WS-CONCON-SCODRU                PIC 9(05) VALUE ZEROS.        VHF0004
       01 WS-SUSCON-SCODRU                PIC 9(11) VALUE ZEROS.
       01 WS-APRCIU-SCODRU                PIC X(16) VALUE ZEROS.
       01 WS-NUMEVA-SCODRU                PIC 9(01) VALUE ZEROS.
       01 WS-CALIF-SCODRU                 PIC S9(02)V99   VALUE ZEROS.
       01 WS-ITEM-AUX-SCODRU              PIC 9(02) VALUE ZEROS.
       01 WS-ALMORACT-SCODRU              PIC 9(06) VALUE ZEROS.        VHF0002 
       01 WS-ALMORHIS-SCODRU              PIC 9(06) VALUE ZEROS.        VHF0002 
       01 WS-ACUM-VALORES-SCODRU.
          03 WS-CTANRE-SCODRU             PIC 9(06) VALUE ZEROS.
          03 WS-MORNRE-SCODRU             PIC 9(06) VALUE ZEROS.
          03 WS-CTAVAL-SCODRU             PIC 9(06) VALUE ZEROS.
          03 WS-VALDIS-SCODRU             PIC 9(13) VALUE ZEROS.
          03 WS-VALSAL-SCODRU             PIC 9(13) VALUE ZEROS.
          03 WS-VALMOR-SCODRU             PIC 9(13) VALUE ZEROS.
          03 WS-VALCUO-SCODRU             PIC 9(13) VALUE ZEROS.
ENDEUD    03 WS-PRIMER-REG-SCODRU         PIC 9     VALUE ZEROS.        VHF0008
       01 WS-CALCULODEF-SCODRU.
VALCON    03 WS-SUM-CAL-X-PESO-SCODRU     PIC S9(13)V99   VALUE ZEROS.  VHF0004
VALCON    03 WS-SUM-PESO-SCODRU           PIC 9(13) VALUE ZEROS.        VHF0004
          03 WS-PESO-SCODRU               PIC 9(03) VALUE ZEROS.

REMARK*** PARAMETROS DE ENTRADA
REMARK*** =====================
REMARK*** CODIGO SUSCRIPTOR QUE SOLICITA EL SCORING
       01 WS-CODSUS-SCODRU                PIC 9(06) VALUE ZEROS.
       01 WS-TIPSCO-SCODRU                PIC 9(01) VALUE ZEROS.
       01 WS-CODSCO-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-FUTEND-SCODRU                PIC 9(06) VALUE ZEROS.
       01 WS-FUMSRE-SCODRU                PIC 9(06) VALUE ZEROS.
       01 WS-RESCON-SCODRU                PIC 9(01) VALUE ZEROS.
       01 WS-IDECIU-SCODRU.
          03 WS-TIDCIU-SCODRU             PIC 9(01) VALUE ZEROS.
          03 WS-NIDCIU-SCODRU             PIC 9(11) VALUE ZEROS.
          03 WS-APECIU-SCODRU             PIC X(16) VALUE SPACES.
          03 WS-VIGCED-SCODRU             PIC 9(02) VALUE ZEROS.
TIPO6  01 WS-VALAPE-SCODRU                PIC 9(01) VALUE ZEROS.        VHF0005

REMARK*** PARAMETROS DE SALIDA
REMARK*** ====================
       01 WS-EVALUACION-SCODRU            PIC X(10) VALUE SPACES.
       01 WS-CODRET-SCODRU                PIC 9(02) VALUE ZEROS.
       01 WS-CALIF-DEF-SCODRU             PIC S9(02)V99   VALUE ZEROS.
       01 WS-CALIF-REL-SCODRU             PIC S9(02)V99   VALUE ZEROS.
       01 WS-EVAL-DEF-SCODRU              PIC 9(01) VALUE ZEROS.
       01 WS-CODEVA-SCODRU                PIC X(01) VALUE SPACES.

REMARK*** TABLA RAZONES DEFINITIVAS
       01 WS-RAZONESDEF-SCODRU.
          03 WS-RAZDEF-SCODRU             OCCURS 10 TIMES.
             05 WS-IDERAZDF-SCODRU        PIC X(01).
             05 WS-TABRAZDF-SCODRU        PIC X(02).
             05 WS-ITERAZDF-SCODRU        PIC X(01).

REMARK*** CONSTANTES
REMARK*** ==========
       01 WS-NO-SCODRU                    PIC 9(01) VALUE 0.
       01 WS-SI-SCODRU                    PIC 9(01) VALUE 1.
       01 WS-MILES-SCODRU                 PIC 9(04) VALUE 1000.
       01 WS-INDEMB-SCODRU                PIC 9(02) VALUE 06.
       01 WS-NUMTAB-SCODRU                PIC 9(02) VALUE 14.
       01 WS-LON-VALNREP-SCODRU           PIC 9(02) VALUE 17.
       01 WS-TIPPERSONAS-SCODRU.
          03 WS-PNATURAL-SCODRU           PIC 9(01) VALUE 1.
          03 WS-PJURIDICA-SCODRU          PIC 9(01) VALUE 2.
       01 WS-ENDEUDAMIENTO-SCODRU.
          03 WS-GRPEND-SCODRU             PIC X(05) VALUE "HMS".
          03 WS-CALEND-SCODRU             PIC X(05) VALUE "ABCDE".
          03 WS-NCAEND-SCODRU             PIC 9(02) VALUE 05.
       01 WS-SECTORREAL-SCODRU.
          03 WS-NECSRE-SCODRU             PIC 9(02) VALUE 06.
       01 WS-INDICESTABESP-SCODRU.
          03 WS-INDCON-SCODRU             PIC 9(02) VALUE 01.
          03 WS-INDDIS-SCODRU             PIC 9(02) VALUE 02.
          03 WS-INDSAL-SCODRU             PIC 9(02) VALUE 03.
          03 WS-INDMOR-SCODRU             PIC 9(02) VALUE 04.
          03 WS-INDCUO-SCODRU             PIC 9(02) VALUE 05.
          03 WS-INDFAL-SCODRU             PIC 9(02) VALUE 09.
          03 WS-INDAMA-SCODRU             PIC 9(02) VALUE 10.           VHF0002 
          03 WS-INDAMH-SCODRU             PIC 9(02) value 11.           VHF0002 

REMARK*** VARIABLES UTILIZADAS PARA MANEJO DE PARAMETROS
       01 WS-PARAMETROS-SCODRU.
          03 WS-ENCABEZADOS-INI-SCODRU.
             05 WS-ENC-TIPPER-SCODRU      PIC X(16) VALUE
                "SCORETIPOPERSONA".
             05 WS-ENC-EVAVAL-SCODRU      PIC X(13) VALUE
                "EVALUAVALORES".
          03 WS-PARAMETROS-INI-SCODRU.
             05 WS-PAR-APLSCO-SCODRU      PIC X(11) VALUE
                "APLICASCORE".
             05 WS-PAR-VALNREP-SCODRU     PIC X(16) VALUE
                "VALORNOREPORTADO".
             05 WS-PAR-ADJCORR-SCODRU     PIC X(16) VALUE               VHF0006
                "ADJCARTERACORREG".                                     VHF0006
          03 WS-VALORES-INI-SCODRU.
             05 WS-VAL-GENERAL-SCODRU     PIC X(07) VALUE
                "GENERAL".
             05 WS-VAL-DIFERENC-SCODRU    PIC X(12) VALUE
                "DIFERENCIADO".
             05 WS-VAL-CEROS-SCODRU       PIC X(05) VALUE
                "CEROS".
             05 WS-VAL-PROMEDIO-SCODRU    PIC X(08) VALUE
                "PROMEDIO".
             05 WS-VAL-EXTODOS-SCODRU     PIC X(12) VALUE
                "EXISTENTODOS".
             05 WS-VAL-ADJCORR-SCODRU     PIC X(02) VALUE               VHF0006
                "SI".                                                   VHF0006
          03 WS-LON-TIPPER-SCODRU         PIC 9(02) VALUE 18.
          03 WS-LON-EVAVAL-SCODRU         PIC 9(02) VALUE 15.
          03 WS-LON-APLSCO-SCODRU         PIC 9(02) VALUE 12.
          03 WS-TABGEN-SCODRU             PIC 9(01) VALUE 1.
          03 WS-TABDIF-SCODRU             PIC 9(01) VALUE 2.
          03 WS-VALCER-SCODRU             PIC 9(01) VALUE 3.
          03 WS-VALPRO-SCODRU             PIC 9(01) VALUE 4.
          03 WS-VALEXT-SCODRU             PIC 9(01) VALUE 5.

REMARK*** CODIGOS DE LAS SUBTABLAS DEL ESQUEMA DE PARAMETROS
       01 WS-SUBTABLAS-SCODRU.
          03 WS-TIPO-EVALUACION-SCODRU    PIC 9(02) VALUE 01.
          03 WS-TIPCTA-HABPAGO-SCODRU     PIC 9(02) VALUE 02.
          03 WS-TIPCTA-ENDEUDAM-SCODRU    PIC 9(02) VALUE 03.
          03 WS-TIPCTA-SECREAL-SCODRU     PIC 9(02) VALUE 04.
          03 WS-TIPCTA-ESPECIAL-SCODRU    PIC 9(02) VALUE 05.
          03 WS-CALIF-CARTERA-SCODRU      PIC 9(02) VALUE 06.
          03 WS-CALIF-CTACTE-SCODRU       PIC 9(02) VALUE 07.
          03 WS-CALIF-ENDEUDAM-SCODRU     PIC 9(02) VALUE 08.
          03 WS-CALIF-SECREAL-SCODRU      PIC 9(02) VALUE 09.
          03 WS-CALIF-ESPECIAL-SCODRU     PIC 9(02) VALUE 10.
          03 WS-CALIF-ADJCAR-SCODRU       PIC 9(02) VALUE 11.
          03 WS-CALIF-ADJCTE-SCODRU       PIC 9(02) VALUE 12.
          03 WS-ESTADO-CARTERA-SCODRU     PIC 9(02) VALUE 13.
          03 WS-ESTADO-CTACTE-SCODRU      PIC 9(02) VALUE 14.
CLAS01    03 WS-RECLAS-TIPEVA-SCODRU      PIC 9(02) VALUE 15.           RMB0007 
          03 WS-ARCH-PARAMETROS-SCODRU    PIC 9(02) VALUE 20.

REMARK*** TIPOS DE SUBTABLAS EN ESQUEMA DE PARAMETROS
       01 WS-COD-TIPTABLAS-SCODRU.
          03 WS-TT-TIPEVA-SCODRU          PIC 9(01) VALUE 1.
          03 WS-TT-TIPCTA-SCODRU          PIC 9(01) VALUE 2.
          03 WS-TT-CALIFI-SCODRU          PIC 9(01) VALUE 3.
          03 WS-TT-ESTUVO-SCODRU          PIC 9(01) VALUE 4.
CLAS01    03 WS-TT-TIPREC-SCODRU          PIC 9(01) VALUE 5.            RMB0007 
          03 WS-TT-ARCPAR-SCODRU          PIC 9(01) VALUE 9.

REMARK*** CODIGOS DE RETORNO (INCLUYE LOS ERRORES RETORNADOS)
       01 WS-CODIGOS-RETORNO-SCODRU.
          03 WS-EXITOSO-SCODRU            PIC 9(02) VALUE 00.
          03 WS-ERR-SINARI-SCODRU         PIC 9(02) VALUE 01.
          03 WS-ERR-SINTBL-SCODRU         PIC 9(02) VALUE 02.
          03 WS-ERR-PARTIPPER-SCODRU      PIC 9(02) VALUE 03.
          03 WS-ERR-PAREVAVAL-SCODRU      PIC 9(02) VALUE 04.
          03 WS-ERRCOD-SUBTAB-SCODRU      PIC 9(02) VALUE 05.
          03 WS-ERRIND-SUBTAB-SCODRU      PIC 9(02) VALUE 06.
          03 WS-ERRBLQ-SCORE-SCODRU       PIC 9(02) VALUE 07.

REMARK*** ITEMS ASOCIADOS A CADA TABLA PARA MOSTRAR RAZONES.
REMARK*** 1:a, 2:b, ...., 26:z
       01 WS-ITEMS-TABLAS-SCODRU.
          03 FILLER                       PIC X(26) VALUE
             "abcdefghijklmnopqrstuvwxyz".
       01 WS-ITEMS-TABLAS-R-SCODRU        REDEFINES
          WS-ITEMS-TABLAS-SCODRU.
          03 WS-ITEM-TAB-SCODRU           PIC X(01) OCCURS 26 TIMES.

 EMARK*** TABLA DE ALTURAS DE MORA ACTUAL PARA ESTADOS DE CARTERA       VHF0002 
       01 WS-ALTURAMORA-ACT-SCODRU.                                     VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000003006009012000000".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000000000".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000000000".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000000000".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000000000".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000000000".      VHF0002 
       01 WS-ALTURAMORA-ACT-SCODRU-R      REDEFINES                     VHF0002 
          WS-ALTURAMORA-ACT-SCODRU.                                     VHF0002 
          03 WS-ALMOAC-SCODRU             PIC 9(03) OCCURS 100 TIMES.   VHF0002 

 EMARK*** TABLA DE ALTURAS DE MORA HISTORICA PARA ESTADOS DE CARTERA    VHF0002 
       01 WS-ALTURAMORA-HIS-SCODRU.                                     VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000003006".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00901200000000000000000000000000000000000000000000".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000000000".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000000000".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000000000".      VHF0002 
          03 FILLER                       PIC X(50) VALUE               VHF0002 
             "00000000000000000000000000000000000000000000000000".      VHF0002 
       01 WS-ALTURAMORA-HIS-SCODRU-R      REDEFINES                     VHF0002 
          WS-ALTURAMORA-HIS-SCODRU.                                     VHF0002 
          03 WS-ALMOHI-SCODRU             PIC 9(03) OCCURS 100 TIMES.   VHF0002 

REMARK*** VARIABLES DE CONTROL
REMARK*** ====================
       01 WS-FDA-SCODTB-SCODRU            PIC 9(01) VALUE ZEROS.
       01 WS-ENCNIT-SCODRU                PIC 9(01) VALUE ZEROS.
       01 WS-ENCDAS-SCODRU                PIC 9(01) VALUE ZEROS.
       01 WS-IND-TABEVA-SCODRU            PIC 9(01) VALUE ZEROS.
       01 WS-IND-EVAVAL-SCODRU            PIC 9(01) VALUE ZEROS.
       01 WS-ENCENC-SCODRU                PIC 9(01) VALUE ZEROS.
       01 WS-ENCNPA-SCODRU                PIC 9(01) VALUE ZEROS.
       01 WS-TODVAL-SCODRU                PIC 9(01) VALUE ZEROS.
       01 WS-CONVAL-SCODRU                PIC 9(01) VALUE ZEROS.
       01 WS-IND-IDENTID-SCODRU           PIC 9(01) VALUE ZEROS.
       01 WS-IND-TIPO1-SCODRU             PIC 9(01) VALUE ZEROS.
       01 WS-IND-RAZDUP-SCODRU            PIC 9(01) VALUE ZEROS.        VHF0003
       01 WS-IND-ADJCAR-SCODRU            PIC 9(01) VALUE ZEROS.        VHF0006

REMARK*** FECHAS
REMARK*** ======
VALCON 01 WS-FECCON-SCODRU                PIC X(08).                    VHF0004
VALCON 01 WS-FECCON-SCODRU-R              REDEFINES  WS-FECCON-SCODRU.  VHF0004
VALCON    03 WS-ANMECO-SCODRU             PIC 9(06).                    VHF0004
VALCON    03 WS-ANMECO-SCODRU-R           REDEFINES  WS-ANMECO-SCODRU.  VHF0004
VALCON       05 WS-ANOCON-SCODRU          PIC 9(04).                    VHF0004
VALCON       05 WS-MESCON-SCODRU          PIC 9(02).                    VHF0004
VALCON    03 FILLER                       PIC X(02).                    VHF0004
VALCON 01 WS-FAPAUX-SCODRU                PIC 9(04).                    VHF0004
VALCON 01 WS-FAPAUX-SCODRU-R              REDEFINES  WS-FAPAUX-SCODRU.  VHF0004
VALCON    03 WS-MAPAUX-SCODRU             PIC 9(02).                    VHF0004
VALCON    03 WS-AAPAUX-SCODRU             PIC 9(02).                    VHF0004
VALCON 01 WS-FCBCON-SCODRU                PIC 9(06).                    VHF0004
VALCON 01 WS-FCBCON-SCODRU-R              REDEFINES  WS-FCBCON-SCODRU.  VHF0004
VALCON    03 WS-DCBCON-SCODRU             PIC 9(02).                    VHF0004
VALCON    03 WS-MCBCON-SCODRU             PIC 9(02).                    VHF0004
VALCON    03 WS-ACBCON-SCODRU             PIC 9(02).                    VHF0004
VALCON 01 WS-FCAAMM-SCODRU                PIC 9(06).                    VHF0004
VALCON 01 WS-FCAAMM-SCODRU-R              REDEFINES  WS-FCAAMM-SCODRU.  VHF0004
VALCON    03 WS-SGAAMM-SCODRU             PIC 9(02).                    VHF0004
VALCON    03 WS-ANAAMM-SCODRU             PIC 9(02).                    VHF0004
VALCON    03 WS-MEAAMM-SCODRU             PIC 9(02).                    VHF0004
