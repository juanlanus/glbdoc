REMARK* DTRLLA-WS.v.0002
REMARK* VARIABLES DE TRABAJO DE LA RUTINA DE CREACION DE CUENTAS PUENTE
REMARK*****************************************************************
REMARK* FAC0002 * DATCIU * DEFINICION DE NUEVA VARIABLE PARA CONTROL 
REMARK* F.CUADROS  * DE ERRORES PARA INTERFASE CON NOVEDAT
REMARK* JUL.02/2003*     *
REMARK*****************************************************************
DTRLLA 01 VARIABLES-DTRLLA.
DTRLLA    03 WS-ERROR-DTRLLA            PIC 9(01) VALUE 0.
DTRLLA    03 WS-GENERAR-PUENTE-DTRLLA   PIC 9(01) VALUE 0.
DTRLLA    03 WS-WRIMAL-DTRLLA           PIC 9(08) VALUE 0.
DTRLLA    03 WS-REWMAL-DTRLLA           PIC 9(08) VALUE 0.
DTRLLA    03 WS-REGMALCOM-DTRLLA        PIC 9(08) VALUE 0.
DTRLLA    03 WS-WRIOK-DTRLLA            PIC 9(08) VALUE 0.
DTRLLA    03 WS-REWOK-DTRLLA            PIC 9(08) VALUE 0.
DTRLLA    03 WS-PROGRAMA-DTRLLA         PIC X(08) VALUE "RENUMERA".
DTRLLA    03 WS-TIPO-EJE-DTRLLA         PIC X     VALUE SPACES.    
DATCIU    03 WS-ERROR-RECLACOM          PIC 9(01) VALUE 0.              FAC0002
