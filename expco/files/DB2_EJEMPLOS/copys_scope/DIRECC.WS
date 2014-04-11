******* DIRECC-WS.v.0010
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES
******* QUE UTILIZAN LAS RUTINAS DE CREACION DE DIRECCIONES         
***********************************************************************
REMARK* VHF0004     * SE ADICIONAN NOMBRES DE ARCHIVOS DE              *
REMARK* VICTOR HUGO * ESTANDARIZACION DE DIRECCIONES                  *
REMARK* FORERO      *                                                  *
REMARK* JUL.08/2002 *                                                  *
REMARK******************************************************************
REMARK* FAC0005     * HDIR01 * INCLUSION RUTA PARA MAESTRO HISTORICO DE*
REMARK* F.CUADROS C.* DIRECCIONES                                      *
REMARK* MAR.04/2003 *                                                  *
***********************************************************************
REMARK* FAC0006     * BDLLOC * AMPLIACION DE LAS OCURRENCIAS DEL VECTOR*
REMARK* F.CUADROS C.* T-LOCALIZACION PARA NUEVOS CAMPOS DE MAESTROS DEL*
REMARK* DIC.11/2003 * CLUB BDLCLI
***********************************************************************
REMARK* FAC0007     * AMPDIR * AMPLIACIÓN DE LA ESTRUCTURA DE ARCHIVOS*
REMARK* F.CUADROS C.* ICBDIR Y BDUHDIR. SE AMPLIAN LOS TAMAÑOS DE LOS *
REMARK* FEB.14/2006 * CAMPOS: TELÉFONOS, DIRECCIONES, CIUDADES, DPTOS.*
***********************************************************************
REMARK* VHF0008     * RCNCER - SE INCLUYE TITULO DE ARCHIVO DE        *
REMARK* VICTOR HUGO *          ACEPCIONES DE CIUDADES PARA OPTIMIZAR  *
REMARK* FORERO      *          PRODUCTO RECONOCER.                    *
REMARK* JUL.06/2006 *                                                 *
REMARK* FAC0008     *          SE INCLUYEN CONTROLES EN HISTÓRICO PARA*
REMARK* JUL.18/2006 *          GEORREFERENCIACIÓN                     *
REMARK*****************************************************************
REMARK* CAV0009      * BDACSV - INCLUIR INDICADOR PARA DETERMINAR SI  *
REMARK* C VELASCO    *          EL REGISTRO CORRESPONDE A UNA CUENTA  *
REMARK* AGO/27/2007  *          SIN VALIDAR IDENTIFICACION            *
REMARK*****************************************************************
REMARK* VHF0010      * FASEII - SE ADICIONAN NUEVAS VARIABLES PARA EL  *
REMARK* VICTOR HUGO  *          ACTUALIZADOR                           *
REMARK* FORERO       *                                                 *
REMARK* JUL.25/2008  *                                                 *
REMARK******************************************************************
DIRECC 01 CONTADORES-DIRECC.
DIRECC    03 TOTAL-DIRECC            PIC 9(8) VALUE 0.
DIRECC    03 CAMBIOS-DIRECC          PIC 9(8) VALUE 0.
DIRECC    03 SINCAMBIOS-DIRECC       PIC 9(8) VALUE 0.
DIRECC    03 CREACIONES-DIRECC       PIC 9(8) VALUE 0.
DIRECC    03 ERRORES-DIRECC          PIC 9(8) VALUE 0.
DIRECC    03 CAMPOSVACIOS-DIRECC     PIC 9(8) VALUE 0.
DIRECC    03 MENOSACTUAL-DIRECC      PIC 9(8) VALUE 0.
DIRECC    03 ERRLLAVE-DIRECC         PIC 9(8) VALUE 0.
DIRECC 01 VARIABLES-DIRECC.
DIRECC    03 IND-DIRECC              PIC 9 VALUE 0.
DIRECC    03 FECHA6-DIRECC           PIC 9(06).
DIRECC    03 ERROR-DIRECC            PIC 9 VALUE 0.
DIRECC    03 CAMBIO-DIRECC           PIC 9 VALUE 0.
BDACSV    03 REG-CSV-DIRECC          PIC 9(1).                          CAV0009 
DIRECC    03 MAES-DIRECC-TITLE PIC X(50) VALUE "$DIRECCIONES/ICBDIR".
          03 TITLE-DIREST            PIC X(40) VALUE                    VHF0004
             "$DIRECCIONES/DIREST".                                     VHF0004
          03 TITLE-DIRCON            PIC X(40) VALUE                    VHF0004
             "$DIRECCIONES/DIRCON".                                     VHF0004
DIRECC    03 VALLOC-DIRECC.
BDLLOC       04 T-LOCALIZACION OCCURS 15 TIMES PIC 9.                   FAC0006
HDIR01    03 MAES-BDUHDIR-TITLE PIC X(20) VALUE "$DIRECCIONES/BDUHDIR". FAC0005 
RCNCER    03 TITLE-BDLSTDCIU    PIC X(80)  VALUE "$DATABASE/BDLSTDCIU". VHF0008 
       01 VARIABLES-TRAB-DIRECC.                                        FAC0003
AMPDIR    03 TEL-W1                  PIC X(12).                         FAC0007 
AMPDIR    03 TEL-W2                  PIC X(12).                         FAC0007 
AMPDIR    03 TEL-WW2                 PIC X(12).                         FAC0007 
          03 CAR-W                   PIC 9(1).                          FAC0003
          03 CAR-WW                  PIC 9(2).                          FAC0003
          03 NUM-CAR-W               PIC 9(2).                          FAC0003
FASEII 01 VARIABLES-NEW-DIRECC.                                         VHF0010 
FASEII    03 SW-HACER-CAMBIO         PIC 9.                             VHF0010 
FASEII    03 DIR-ACTUAL              PIC X(60).                         VHF0010 
FASEII    03 DIR-NUEVA               PIC X(60).                         VHF0010 
HDIR01 01 REG-BDUHDIR-WS.                                               FAC0005 
HDIR01    03  LLAVE-BDUHDIR-WS.                                         FAC0005 
HDIR01        05  LLAVE-PRI-BDUHDIR-WS.                                 FAC0005 
HDIR01            07  TIPO-ID-BDUHDIR-WS    PIC 9(01).                  FAC0005 
AMPDIR            07  NUM-ID-BDUHDIR-WS     PIC 9(13).                  FAC0007 
HDIR01        05  LLAVE-ALT-BDUHDIR-WS.                                 FAC0005 
HDIR01            07  COD-SUS-BDUHDIR-WS    PIC 9(06).                  FAC0005 
HDIR01            07  COD-CTA-BDUHDIR-WS    PIC 9(02).                  FAC0005 
HDIR01            07  NUM-CTA-BDUHDIR-WS    PIC X(18).                  FAC0005 
HDIR01        05  LLAVE-CONSEC-BDUHDIR-WS.                              FAC0005 
HDIR01            07  NUM-CONSEC-BDUHDIR-WS PIC 9(3).                   FAC0005 
AMPDIR     03  FECAPE-BDUHDIR-WS            PIC 9(06) COMP-3.           FAC0007 
AMPDIR     03  FECCRE-BDUHDIR-WS            PIC 9(06) COMP-3.           FAC0007 
AMPDIR     03  FECACT-BDUHDIR-WS            PIC 9(06) COMP-3.           FAC0007 
AMPDIR     03  CIURES-BDUHDIR-WS            PIC X(20).                  FAC0007 
AMPDIR     03  DPTRES-BDUHDIR-WS            PIC X(20).                  FAC0007 
AMPDIR     03  DIRRES-BDUHDIR-WS            PIC X(60).                  FAC0007 
AMPDIR     03  TELRES-BDUHDIR-WS            PIC 9(12) COMP-3.           FAC0007 
FASEII*    03  FECRES-BDUHDIR-WS            PIC 9(06) COMP-3.           VHF0010 
FASEII     03  FECDRES-BDUHDIR-WS           PIC 9(06) COMP-3.           VHF0010 
AMPDIR     03  CIULAB-BDUHDIR-WS            PIC X(20).                  FAC0007 
AMPDIR     03  DPTLAB-BDUHDIR-WS            PIC X(20).                  FAC0007 
AMPDIR     03  DIRLAB-BDUHDIR-WS            PIC X(60).                  FAC0007 
AMPDIR     03  TELLAB-BDUHDIR-WS            PIC 9(12) COMP-3.           FAC0007 
FASEII*    03  FECLAB-BDUHDIR-WS            PIC 9(06) COMP-3.           VHF0010 
FASEII     03  FECDLAB-BDUHDIR-WS           PIC 9(06) COMP-3.           VHF0010 
AMPDIR     03  CIUCOR-BDUHDIR-WS            PIC X(20).                  FAC0007 
AMPDIR     03  DPTCOR-BDUHDIR-WS            PIC X(20).                  FAC0007 
AMPDIR     03  DIRCOR-BDUHDIR-WS            PIC X(60).                  FAC0007 
FASEII*    03  FECCOR-BDUHDIR-WS            PIC 9(06) COMP-3.           VHF0010 
FASEII     03  FECDCOR-BDUHDIR-WS           PIC 9(06) COMP-3.           VHF0010 
AMPDIR     03  E-MAIL-BDUHDIR-WS            PIC X(40).                  FAC0007 
AMPDIR     03  TELCEL-BDUHDIR-WS            PIC 9(12) COMP-3.           FAC0007 
FASEII*    03  FECOTR-BDUHDIR-WS            PIC 9(06) COMP-3.           VHF0010 
FASEII     03  FECCEL-BDUHDIR-WS            PIC 9(06) COMP-3.           VHF0010 
AMPDIR     03  FECSIS-BDUHDIR-WS            PIC 9(06) COMP-3.           FAC0007 
AMPDIR     03  FECSUS-BDUHDIR-WS            PIC 9(06) COMP-3.           FAC0007 
AMPDIR     03  FECULT-BDUHDIR-WS            PIC 9(06) COMP-3.           FAC0007 
RCNCER     03  CERRES-BDUHDIR-WS            PIC 9(01).                  FAC0008 
RCNCER     03  ADIRES-BDUHDIR-WS            PIC 9(02).                  FAC0008 
RCNCER     03  CERLAB-BDUHDIR-WS            PIC 9(01).                  FAC0008 
RCNCER     03  ADILAB-BDUHDIR-WS            PIC 9(02).                  FAC0008 
RCNCER     03  CERCOR-BDUHDIR-WS            PIC 9(01).                  FAC0008 
RCNCER     03  ADICOR-BDUHDIR-WS            PIC 9(02).                  FAC0008 
BDACSV     03  INDCSV-BDUHDIR-WS            PIC 9(01).                  CAV0009 
FASEII     03  FECTRES-BDUHDIR-WS           PIC 9(06) COMP-3.           VHF0010 
FASEII     03  FECTLAB-BDUHDIR-WS           PIC 9(06) COMP-3.           VHF0010 
FASEII     03  FECMAIL-BDUHDIR-WS           PIC 9(06) COMP-3.           VHF0010 
FASEII*    03  FILLER-BDUHDIR-WS            PIC X(40).                  VHF0010 
FASEII     03  FILLER-BDUHDIR-WS            PIC X(28).                  VHF0010 
