      * BDIIALE-WS.v01.0005
REMARK* VARIABLES RELACIONADAS CON LAS ALERTAS DEL SISTEMA           
REMARK**************************************************************************
REMARK* RMB0002   * TIPALE: CREACION DE DIFERENTES TIPOS DE ALERTA, QUE SE 
REMARK* R.MARTINEZ* PUEDEN GENERAR SOBRE DIFERENTES TIPOS DE INFORMACION
REMARK* ABRIL/2006* (ARCHIVOS)
REMARK*************************************************************************
REMARK* RMB0003   * CIU001: AJUSTES PARA MANEJO DE ALERTAS DE CIUDADANOS DE 
REMARK* R.MARTINEZ* COLOMBIA.                                            
REMARK* NOV./2006 * 
REMARK*************************************************************************
REMARK* VHF0004     * REFCOM - SE AJUSTA EL TAMAÑO DE LAS LINEAS DE    *
REMARK* VICTOR HUGO *          DETALLE DE ALERTA YA QUE SU TAMAÑO      *
REMARK* FORERO      *          ESTABA DE 81 Y NO DE 80 QUE ES ESTANDAR *
REMARK* OCT.29/2008 *                                                  *
REMARK******************************************************************
REMARK* GJC0005     * LEY001 - SE CAMBIA EL GUIN INICIAL DE LA PRESENTACION
REMARK* G.JIMENEZ   *          DE LA ALEERTA POR UN ESPACIO EN BLANCO. ESTO
REMARK* MARZO 2/2009*          PARA EVITAR QUE SE INTERPRETE COMO ALGO  
REMARK*             *          NEGATIVO
remark******************************************************************
       01 VARIABLES-BDIIALE.
CIU001    02 MAES-BDIIALE-TITLE    PIC X(20)                            RMB0003 
CIU001                             VALUE "$DATABASE/BDIIALE".           RMB0003 
          02 EST-BDIIALE           PIC X(02) VALUE "00".
          02 FIN-BDIIALE           PIC 9(01) VALUE 0.
TIPALE    02 PROCESAR-BDIIALE      PIC 9(01) VALUE 0.                   RMB0002
          02 CONT-LINEAS-BDIIALE   PIC 9(03) VALUE 0.
          02 ABIERTO-BDIIALE       PIC 9(01) VALUE 0.
          02 TIP-IDE-AUX-BDIIALE   PIC 9(01) VALUE 0.
          02 NUM-IDE-AUX-BDIIALE   PIC 9(11) VALUE 0.
REFCOM    02 MARCAR-ICBTCO-BDIIALE PIC 9(01) VALUE 0.                   VHF0004
CIU001    02 FECHA-AUX-BDIIALE     PIC 9(08) VALUE 0.                   RMB0003 
CIU001    02 FECHA-NOV-AUX-BDIIALE PIC 9(08) VALUE 0.                   RMB0003 
CIU001    02 FECHA-VEN-AUX-BDIIALE PIC 9(08) VALUE 0.                   RMB0003 
CIU001    02 FECHA-REP-AUX-BDIIALE PIC 9(08) VALUE 0.                   RMB0003 
CIU001    02 FUENTE-AUX-BDIIALE    PIC 9(06) VALUE 0.                   RMB0003 
CIU001    02 NOMFUE1-AUX-BDIIALE   PIC X(14) VALUE SPACES.              RMB0003 
CIU001    02 NOMFUE2-AUX-BDIIALE   PIC X(14) VALUE SPACES.              RMB0003 
CIU001    02 CODALE-AUX-BDIIALE    PIC 9(03) VALUE 0.                   RMB0003 
CIU001    02 TIPALE-AUX-BDIIALE    PIC X(01) VALUE SPACES.              RMB0003 
CIU001    02 TEXTO1-AUX-BDIIALE    PIC X(50) VALUE SPACES.              RMB0003 
CIU001    02 TEXTO2-AUX-BDIIALE    PIC X(50) VALUE SPACES.              RMB0003 
CIU001    02 INF-DATAHOST-AUX-BDIIALE      PIC X(18) VALUE SPACES.      RMB0003 
TIPALE    02 PAR-CODBLOALER        PIC 9(1)  VALUE 1.                   RMB0002 
TIPALE    02 PAR-MSGBLOALER        PIC X(60) VALUE                      RMB0002 
TIPALE       " ALERTA DADA POR LA ENTIDAD".                             RMB0002 
TIPALE 01 M-ALERTA.                                                     RMB0002 
TIPALE    03 V-ALERTA   OCCURS 40 TIMES PIC 9(02).                      RMB0002 
       01 TITULOS-BDIIALE.
          02 FILLER                PIC X(30) VALUE ALL "=".
          02 FILLER    PIC X(20) VALUE "==== [ ALERTAS ] ===".
          02 FILLER                PIC X(30) VALUE ALL "=".
          02 FILLER    PIC X(20) VALUE "FUENTE           FEC".
          02 FILLER    PIC X(20) VALUE "HA           DESCRIP".
          02 FILLER    PIC X(20) VALUE "CION                ".
          02 FILLER    PIC X(20) VALUE "                    ".
          02 FILLER                PIC X(80) VALUE ALL "=".
CIU001 01 LINEA1-BDIIALE-TIT.                                           RMB0003 
LEY001    02 RAYA-BDIIALE          PIC X(01) VALUE " ".                 GJC0005 
CIU001    02 NOMFUE-LIN1-BDIIALE   PIC X(14) VALUE SPACES.              RMB0003 
CIU001    02 FILLER                PIC X(01) VALUE SPACES.              RMB0003 
CIU001    02 FEC-NOV-LIN1-BDIIALE  PIC X(11) VALUE SPACES.              RMB0003 
CIU001    02 FILLER                PIC X(01) VALUE SPACES.              RMB0003 
CIU001    02 TEXTO-LIN1-BDIIALE    PIC X(50) VALUE SPACES.              RMB0003 
REFCOM    02 FILLER                PIC X(02) VALUE SPACES.              VHF0004 
CIU001 01 LINEA2-BDIIALE-TIT.                                           RMB0003 
CIU001    02 FILLER                PIC X(01) VALUE SPACES.              RMB0003 
CIU001    02 NOMFUE-LIN2-BDIIALE   PIC X(14) VALUE SPACES.              RMB0003 
CIU001    02 FILLER                PIC X(01) VALUE SPACES.              RMB0003 
CIU001    02 FILLER                PIC X(11) VALUE SPACES.              RMB0003 
CIU001    02 FILLER                PIC X(01) VALUE SPACES.              RMB0003 
CIU001    02 TEXTO-LIN2-BDIIALE    PIC X(50) VALUE SPACES.              RMB0003 
REFCOM    02 FILLER                PIC X(02) VALUE SPACES.              VHF0004 

