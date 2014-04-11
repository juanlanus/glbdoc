******* ICBPAR-WS.v.0004
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES
******* QUE UTILIZAN LA RUTINA PARA MANEJAR PARAMETROS DEL SISTEMA .
***********************************************************************
REMARK* RMB0002     * MONE01 - SE PERMITEN MANEJA PARAMETROS ALFABETICO*RMB0002
REMARK* R.MARTINEZ  *           Y NO SOLO NUMERICOS COMO ESTABA INICIAL*RMB0002
REMARK* NOV.27/2001 *           MENTE                                  *RMB0002
REMARK******************************************************************RMB0002
REMARK* VHF0003     * MONE03 - SE AMPLIA EL TAMANO DEL VALOR NUMERICO A*
REMARK* VICTOR HUGO *          18                                      *
REMARK* FORERO      *                                                  *
REMARK* ABR.15/2004 *                                                  *
REMARK******************************************************************
REMARK* GJC0004     * BUREAL - SE INCLUYE UNA NUEVA VARIABLE PARA IDEN-*
REMARK* G.JIMENEZ   *          TIFICAR EN QUE BURÓ SE ESTA. ESTO SE RE-*
REMARK* AGOSTO/2006 *          QUIRIO PARA EL MANEJO DE 2 BURÓS EN VENEZ.
REMARK*             * ALER01 - VARIABLES INDICADORAS DE ALERTAS, QUE
REMARK*             *          DEFINEN A QUE TIPO DE CONSULTA SE LE PRE-
REMARK*             *          SENTAN LAS ALERTAS
REMARK******************************************************************
INTERN 01 VARIABLES-INTERNACIONALES.
INTERN    02 PAR-PAIS          PIC 99 VALUE 0.
INTERN    02 PAR-ENV-VAL       PIC 99 VALUE 0.
INTERN    02 PAR-MOS-CII       PIC 99 VALUE 0.
INTERN    02 PAR-MOS-PUB       PIC 99 VALUE 0.
INTERN    02 PAR-END-HP        PIC 99 VALUE 0.
INTERN    02 PAR-EST-SIT       PIC 99 VALUE 0.
INTERN    02 PAR-TRI-1         PIC 9(8) VALUE 0.
INTERN    02 PAR-TRI-2         PIC 9(8) VALUE 0.
INTERN    02 PAR-TRI-3         PIC 9(8) VALUE 0.
INTERN    02 PAR-TRI-4         PIC 9(8) VALUE 0.
INTERN    02 PAR-PAIS-AUX      PIC 99 VALUE 0.
INTERN    02 PAR-PAISES.                  
INTERN       03 COLOMBIA       PIC 99    VALUE 01.
INTERN       03 VENEZUELA      PIC 99    VALUE 02.
INTERN       03 ECUADOR        PIC 99    VALUE 03.
INTERN       03 PERU           PIC 99    VALUE 04.
MONE01    02 TAB-PARAMETROS    PIC X(80) OCCURS 100 TIMES.              RMB0002 
INTERN    02 ENCABEZADO-PARAMETRO PIC X(18).
INTERN    02 ENCABEZADO-BUSCO     PIC X(18).
INTERN    02 VARIABLE-PARAMETRO   PIC X(18).
INTERN    02 VARIABLE-BUSCO       PIC X(18).
INTERN    02 ENCONTRO-PAR         PIC 9.     
INTERN    02 LECTURA-PAR          PIC 9 VALUE 0.     
MONE02    02 VALOR-PARAMETRO      PIC 9(18).                            VHF0002 
MONE01    02 VALOR-PARAMETRO-A    PIC X(60).                            RMB0002 
INTERN    02 PAR-I                PIC 9(5). 
INTERN    02 PAR-MSG              PIC 9(2). 
INTERN    02 ICBCII-OPEN          PIC 9 VALUE 0.
INTERN    02 LEYENDA-LIBRE        PIC X(78) OCCURS 10 TIMES.
BUREAL    02 BURO-SYS             PIC 9(2) VALUE 0.                     GJC0004
ALER01    02 ALEWEB-SYS           PIC X(02).                            GJC0004
ALER01    02 ALEDTH-SYS           PIC X(02).                            GJC0004
ALER01    02 ALEXML-SYS           PIC X(02).                            GJC0004
