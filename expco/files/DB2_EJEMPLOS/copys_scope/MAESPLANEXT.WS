REMARK*** MAESPLANEXT-WS.v.0009
REMARK******************************************************************
REMARK*** RUTINA PARA CONVERTIR EN ARCHIVOS PLANOS LOS ARCH. MAESTROS  *
REMARK******************************************************************
REMARK* VHF0002     * SE MODIFICA RUTINA PARA MODIFICACION DE ESTRUCTU-*
REMARK* VICTOR HUGO * RA DE ICMCHK Y PARA REESTRUCTURACION DE ICMCRE   *
REMARK* FORERO      * POR MAPA DE BITS PARA VALORES                    *
REMARK* JUN.14/2002 *                                                  *
REMARK******************************************************************
REMARK* VHF0003     * SE ADICIONA CAMPO VECTOR-2-R-ABD07-WS QUE        *
REMARK* VICTOR HUGO * REEMPLAZO AL CAMPO FILLER-ABD07-WS Y SE AJUSTA LA*
REMARK* FORERO      * ESTRUCTURA PARA INCLUIR EL MAPA DE BITS EN LUGAR *
REMARK* NOV.06/2002 * DEL CAMPO BINARIO.                               *
REMARK******************************************************************
REMARK* VHF0004     * SE MODIFICA ESTRUCTURA PARA MANEJAR CAMPO        *
REMARK* VICTOR HUGO * ART15-ABD0? COMO IND-MODIFICACION-ABD0? Y CAMPO  *
REMARK* FORERO      * RECLAMO-ABD0? COMO IND-RECLAMO-ABD0? Y           *
REMARK* JUN.04/2003 * FILLER-RECLAMO-ABD0?                             *
REMARK******************************************************************
REMARK* FAC0005     * REGLOC * INCLUSION DE CAMPO REG-LOCK-ABD0X PARA
REMARK* F.CUADROS   * CONTROLAR REGISTROS CERRADOS POR NOVEDAT
REMARK* FEB.01/2006 * (VALORES 1-SI 0-NO)
REMARK******************************************************************
REMARK* VHF0006     * LEY007 - SE ELIMINA MANEJODE CAMPOS DE           *
REMARK* VICTOR HUGO *          ENDEUDAMIENTO DEL ARCHIVO ICMCRE QUE SE *
REMARK* FORERO      *          REDEFINIERON PARA LOS NUEVOS CAMPOS DE  *
REMARK* SEP.08/2009 *          LEY                                     *
REMARK******************************************************************
REMARK* CAV0007     * RZN001 - SE CAMBIA FILLER POR RAZON DE BLOQUEO,  *
REMARK* CARLOS      *          EN ICMCRE (ABD07) Y EN ICMCHK (ABD06)   *
REMARK* VELASCO     *          DEBIDO A QUE LOS TIPOS DE BLOQUEO YA    *
REMARK* JUL.05/2011 *          SON MÁS DE 10.                          *
REMARK******************************************************************
REMARK* VHF0008     * RZN002 - SE CORRIGE TAMANO DEL FILLER ACTUALIZADO*
REMARK* VICTOR HUGO *          POR RAZON DE BLOQUEO                    *
REMARK* FORERO      *                                                  *
REMARK* SEP.08/2011 *                                                  *
REMARK* CAV0008     * MICROC - SE REDEFINE CAMPO IND-BLOQUEO-R-ABD07   *
REMARK* C. VELASCO  *          DE PIC 9 A PIC X PARA TIPO GARANTIA.    *
REMARK******************************************************************
REMARK* ABB0009     * NHDC01 * INCLUSION DE CAMPOS NUEVA HISTORIA DE    ABB0009
REMARK* A.BERNAL    * CREDITO                                           ABB0009
REMARK* JUN.01/2009 *                                                   ABB0009
REMARK* VHF0009     * MICHCP * APLICAR CAMBIOS DE MICROCREDITO         *VHF0009
REMARK* VICTOR HUGO *                                                  *VHF0009
REMARK* FORERO      *                                                  *VHF0009
REMARK* NOV.08/2011 *                                                  *VHF0009
REMARK******************************************************************
024800*----------------------------------------------------------------*
       01 CTA-CTE                     PIC 9(02)  VALUE 2.
       01 CARTERA                     PIC 9(02)  VALUE 3.
       01 ENDEUDAMIENTO               PIC 9(02)  VALUE 10.
       01 TIP-CTA-RECLAMO-WS          PIC 9(02).
NHDC01 01 REG-MAESTR-PLANO            PIC X(1435).                      ABB0009
NHDC01 01 REG-MAESTR-PLANO-R          REDEFINES REG-MAESTR-PLANO.       ABB0009
NHDC01    03 COMPLETO-ABD06-PLANO     PIC X(101).                       ABB0009
NHDC01    03 COMPLETO-ICMCHQ-PLANO    PIC X(086).                       ABB0009
NHDC01    03 REGCHQ-ICMVLR-PLANO      PIC X(1117).                      ABB0009
NHDC01    03 FILLER                   PIC X(0131).                      ABB0009
NHDC01 01 REG-MAESTR-PLANO-R1         REDEFINES REG-MAESTR-PLANO.       ABB0009
NHDC01    03 COMPLETO-ABD07-PLANO     PIC X(318).                       ABB0009
NHDC01    03 REG-ICMVLR-PLANO         PIC X(1117).                      ABB0009
       01 MAPA-BITS-WS.                                                 VHF0002
          03 MAP-BIT-WS               PIC 9(01)  OCCURS 15 TIMES.       VHF0002
       01 I-MAESPLAN                  PIC 9(05).                        VHF0002

NHDC01*    REGISTRO MAESTRO CUENTAS CORRIENTES PARA TRABAJO 101         ABB0009
NHDC01 01  COMPLETO-ABD06-WS                PIC X(101).                 ABB0009
NHDC01 01  COMPLETO-ABD06-WS-R REDEFINES COMPLETO-ABD06-WS.             ABB0009
NHDC01     03  LLAVE-MAES-CHECK-WS.                                     ABB0009
NHDC01         05  LLAVE-MAES-CHECK-PRI-WS.                             ABB0009
NHDC01             07  TIPO-ID-ABD06-WS     PIC 9.                      ABB0009
NHDC01             07  NUM-ID-ABD06-WS      PIC 9(11).                  ABB0009
NHDC01         05  LLAVE-MAES-CHECK-ALT-WS.                             ABB0009
NHDC01             07  COD-SUS-ABD06-WS     PIC 9(6).                   ABB0009
NHDC01             07  COD-CTA-ABD06-WS     PIC 99.                     ABB0009
NHDC01             07  NUM-CTA-ABD06-WS     PIC X(18).                  ABB0009
NHDC01     03  IND-BLOQUEO-ABD06-WS         PIC 9.                      ABB0009
NHDC01     03  FECHA-APERT-ABD06-WS         PIC 9(4).                   ABB0009
NHDC01     03  CALIFIC-ABD06-WS             PIC 9.                      ABB0009
NHDC01     03  TIP-CALIFIC-ABD06-WS         PIC 9.                      ABB0009
NHDC01     03  NOV-REPORT-ABD06-WS          PIC 99.                     ABB0009
NHDC01     03  FEC-NOVEDAD-ABD06-WS         PIC 9(4).                   ABB0009
NHDC01     03  NUM-EMBARGO-ABD06-WS         PIC 99.                     ABB0009
NHDC01     03  FEC-ULT-EMB-ABD06-WS         PIC 9(4).                   ABB0009
NHDC01     03  ADJETIVO-ABD06-WS            PIC 9.                      ABB0009
NHDC01     03  FEC-ADJETIVO-ABD06-WS        PIC 9(4).                   ABB0009
NHDC01     03  ART15-ABD06-WS               PIC 9.                      ABB0009
NHDC01     03  IND-MODIFICACION-ABD06-WS    PIC 9                       ABB0009
NHDC01                                      REDEFINES ART15-ABD06-WS.   ABB0009
NHDC01     03  RECLAMO-ABD06-WS             PIC 9(5).                   ABB0009
NHDC01     03  RECLAMO-ABD06-RR-WS          PIC 9(5)                    ABB0009
NHDC01                                      REDEFINES RECLAMO-ABD06-WS. ABB0009
NHDC01     03  RECLAMO-ABD06-R-WS           REDEFINES RECLAMO-ABD06-WS. ABB0009
NHDC01         05 IND-RECLAMO-ABD06-WS      PIC 9(01).                  ABB0009
NHDC01         05 REG-LOCK-ABD06-WS         PIC 9(01).                  ABB0009
NHDC01         05 FILLER-RECLAMO-ABD06-WS   PIC 9(03).                  ABB0009
NHDC01     03  CONSECUTIVO-ABD06-WS         PIC 99.                     ABB0009
NHDC01     03  OFICINA-ABD06-WS             PIC X(15).                  ABB0009
NHDC01     03  CIUDAD-ABD06-WS              PIC X(6).                   ABB0009
NHDC01     03  FEC-NOVANT-ABD06-WS          PIC 9(4).                   ABB0009
NHDC01     03  NOVANT-ABD06-WS              PIC 9(2).                   ABB0009
NHDC01     03  IND-SOBREGIRO-ABD06-WS       PIC 9(1).                   ABB0009
NHDC01     03  COD-EST-TITUL-ABD06-WS       PIC 9(1).                   ABB0009
NHDC01     03  CALIFICACION-ABD06-WS        PIC 9(1).                   ABB0009
NHDC01     03  RAZON-BLOQUEO-ABD06-WS       PIC X(1).                   ABB0009
NHDC01*    REGISTRO MAESTRO CUENTAS CORRIENTES EXT PARA TRABAJO 86      ABB0009
NHDC01 01  COMPLETO-ICMCHQ-WS                PIC X(86).                 ABB0009
NHDC01 01  COMPLETO-ICMCHQ-WS-R REDEFINES COMPLETO-ICMCHQ-WS.           ABB0009
NHDC01     03 LLAVE-MAES-ICMCHQ-WS.                                     ABB0009
NHDC01        07 TIPO-ID-ICMCHQ-WS           PIC 9.                     ABB0009
NHDC01        07 NUM-ID-ICMCHQ-WS            PIC 9(11).                 ABB0009
NHDC01        07 COD-SUS-ICMCHQ-WS           PIC 9(6).                  ABB0009
NHDC01        07 COD-CTA-ICMCHQ-WS           PIC 99.                    ABB0009
NHDC01        07 NUM-CTA-ICMCHQ-WS           PIC X(18).                 ABB0009
NHDC01     03 CLASE-CTA-ICMCHQ-WS            PIC 9(1).                  ABB0009
NHDC01     03 FECHA-APERT-ICMCHQ-WS          PIC 9(8).                  ABB0009
NHDC01     03 FECHA-ADJET-ICMCHQ-WS          PIC 9(8).                  ABB0009
NHDC01     03 FECHA-ESTAD-ICMCHQ-WS          PIC 9(8).                  ABB0009
NHDC01     03 TIPO-MONEDA-ICMCHQ-WS          PIC 9.                     ABB0009
NHDC01     03 CUPO-SOBREG-AUT-ICMCHQ-WS      PIC 9(11).                 ABB0009
NHDC01     03 DIAS-SOBREG-AUT-ICMCHQ-WS      PIC 9(3).                  ABB0009
NHDC01     03 FCH-CUPODIAS-SOBREG-ICMCHQ-WS  PIC 9(8).                  ABB0009
NHDC01*    REGISTRO MAESTRO CUENTAS CARTERA   PARA TRABAJO 318          ABB0009
NHDC01 01  COMPLETO-ABD07-WS                PIC X(318).                 ABB0009
NHDC01 01  COMPLETO-ABD07-WS-R REDEFINES COMPLETO-ABD07-WS.             ABB0009
NHDC01     03  LLAVE-MAES-CREDIT-WS.                                    ABB0009
NHDC01         05  LLAVE-MAES-CREDIT-PRI-WS.                            ABB0009
NHDC01             07  TIPO-ID-ABD07-WS     PIC 9.                      ABB0009
NHDC01             07  NUM-ID-ABD07-WS      PIC 9(11).                  ABB0009
NHDC01         05  LLAVE-MAES-CREDIT-ALT-WS.                            ABB0009
NHDC01             07  COD-SUS-ABD07-WS     PIC 9(6).                   ABB0009
NHDC01             07  COD-CTA-ABD07-WS     PIC 99.                     ABB0009
NHDC01             07  NUM-CTA-ABD07-WS     PIC X(18).                  ABB0009
NHDC01            07  NUM-CTA-VEN16-ABD07-WS REDEFINES NUM-CTA-ABD07-WS.ABB0009
NHDC01                 09  NUM-CTA-VEN-ABD07-WS PIC 9(16).              ABB0009
NHDC01                 09  GARANTE-VEN-ABD07-WS PIC 99.                 ABB0009
NHDC01     03  IND-BLOQUEO-ABD07-WS         PIC 9     .                 ABB0009
NHDC01     03  FECHA-APERT-ABD07-WS         PIC 9(4)  .                 ABB0009
NHDC01     03  FECHA-VENC-ABD07-WS          PIC 9(4)  .                 ABB0009
NHDC01     03  CUANTIA-ABD07-WS             PIC 9(5)  .                 ABB0009
NHDC01     03  CALIFIC-ABD07-WS             PIC 9     .                 ABB0009
NHDC01     03  TIP-CALIFIC-ABD07-WS         PIC 9     .                 ABB0009
NHDC01     03  NOV-REPORT-ABD07-WS          PIC 99    .                 ABB0009
NHDC01     03  FEC-NOVEDAD-ABD07-WS         PIC 9(4)  .                 ABB0009
NHDC01     03  NUM-MORAS30-ABD07-WS         PIC 99    .                 ABB0009
NHDC01     03  ESTADO-MES-ABD07-WS          PIC 9(12) .                 ABB0009
NHDC01     03  VECTOR-HIS-1-ABD07-WS        PIC 9(12)                   ABB0009
NHDC01         REDEFINES                 ESTADO-MES-ABD07-WS.           ABB0009
NHDC01     03  MORA-MAXIMA-ABD07-WS         PIC 9     .                 ABB0009
NHDC01     03  ADJETIVO1-ABD07-WS           PIC 9     .                 ABB0009
NHDC01     03  FEC-MEN-ADJ-ABD07-WS         PIC 9(4)  .                 ABB0009
NHDC01     03  ADJETIVO2-ABD07-WS           PIC 9     .                 ABB0009
NHDC01     03  FEC-MAY-ADJ-ABD07-WS         PIC 9(4)  .                 ABB0009
NHDC01     03  ADJETIVO3-ABD07-WS           PIC 9     .                 ABB0009
NHDC01     03  ART15-ABD07-WS               PIC 9     .                 ABB0009
NHDC01     03  IND-MODIFICACION-ABD07-WS    PIC 9                       ABB0009
NHDC01         REDEFINES                 ART15-ABD07-WS.                ABB0009
NHDC01     03  RECLAMO-ABD07-WS             PIC 9(5)  .                 ABB0009
NHDC01     03  RECLAMO-ABD07-RR-WS          REDEFINES RECLAMO-ABD07-WS  ABB0009
NHDC01                                   PIC 9(5).                      ABB0009
NHDC01     03  RECLAMO-ABD07-R-WS           REDEFINES RECLAMO-ABD07-WS. ABB0009
NHDC01         05 IND-RECLAMO-ABD07-WS      PIC 9(01).                  ABB0009
NHDC01         05 REG-LOCK-ABD07-WS         PIC 9(01).                  ABB0009
NHDC01         05 FILLER-RECLAMO-ABD07-WS   PIC 9(03).                  ABB0009
NHDC01         05 TARJ-ESTADO-PLAST-ABD07-WS    PIC 9(1)   REDEFINES    ABB0009
NHDC01            FILLER-RECLAMO-ABD07-WS.                              ABB0009
NHDC01         05 REAL-TERMINO-CONTRATO-ABD07-WS PIC 9(1)               ABB0009
NHDC01            REDEFINES FILLER-RECLAMO-ABD07-WS.                    ABB0009
NHDC01     03  IND-BLOQUEO-R-ABD07-WS       PIC X     .                 ABB0009
MICROC     03  TIPO-GAR-DETALLE-ABD07-WS PIC X   REDEFINES              CAV0008
MICROC                                IND-BLOQUEO-R-ABD07-WS.           CAV0008
NHDC01     03  CALIFIC-R-ABD07-WS           PIC 9     .                 ABB0009
NHDC01     03  TIP-CALIFIC-R-ABD07-WS       PIC 9     .                 ABB0009
NHDC01     03  IND-MAPA-BITS-VLR-ABD07-WS                               ABB0009
NHDC01                REDEFINES TIP-CALIFIC-R-ABD07-WS                  ABB0009
NHDC01                                   PIC 9     .                    ABB0009
NHDC01     03  NOV-REPORT-R-ABD07-WS        PIC 99    .                 ABB0009
NHDC01     03  FEC-NOVEDAD-R-ABD07-WS       PIC 9(4)  .                 ABB0009
NHDC01     03  NUM-MORAS30-R-ABD07-WS       PIC 9(15) .                 ABB0009
NHDC01     03  MAPA-BITS-VLR-ABD07-WS REDEFINES NUM-MORAS30-R-ABD07-WS  ABB0009
NHDC01                                   PIC 9(15).                     ABB0009
NHDC01     03  ESTADO-MES-R-ABD07-WS        PIC 9(12) .                 ABB0009
NHDC01     03  VECTOR-HIS-3-ABD07-WS        PIC 9(12)                   ABB0009
NHDC01         REDEFINES                 ESTADO-MES-R-ABD07-WS.         ABB0009
NHDC01     03  MORA-MAXIMA-R-ABD07-WS       PIC 9     .                 ABB0009
NHDC01     03  VALOR-INICIAL-ABD07-WS       PIC 9(7)  .                 ABB0009
NHDC01     03  NUEVOS-CMPOS-HSTRIA-CREDITO-WS.                          ABB0009
NHDC01        05  FECHA-APERTURA-ABD07-WS       PIC 9(8)  .             ABB0009
NHDC01        05  FECHA-VENCIMIENTO-ABD07-WS    PIC 9(8)  .             ABB0009
NHDC01        05  FECHA-ADJETIVO1-ABD07-WS      PIC 9(8)  .             ABB0009
NHDC01        05  FECHA-ADJETIVO2-ABD07-WS      PIC 9(8) .              ABB0009
NHDC01        05  FECHA-ADJETIVO3-ABD07-WS      PIC 9(8)  .             ABB0009
NHDC01         05  ESTADO-OBLIG-ABD07-WS         PIC 9(2)  .            ABB0009
NHDC01         05  FECHA-ESTADO-OBLIG-ABD07-WS   PIC 9(8)  .            ABB0009
NHDC01         05  FECHA-ESTADO-ORIGEN-ABD07-WS  PIC 9(8)  .            ABB0009
NHDC01         05  FECHA-LIMITE-PAGO-ABD07-WS    PIC 9(8)  .            ABB0009
NHDC01         05  TARJ-FEC-ESTADO-PLAST-ABD07-WS PIC 9(8) .            ABB0009
NHDC01         05  REAL-FEC-CLAUS-PERM-ABD07-WS  PIC 9(8)               ABB0009
NHDC01             REDEFINES TARJ-FEC-ESTADO-PLAST-ABD07-WS.            ABB0009
NHDC01         05  TARJ-CLASE-ABD07-WS           PIC 9(1)  .            ABB0009
NHDC01         05  TARJ-FRANQUICIA-ABD07-WS      PIC 9(1)  .            ABB0009
NHDC01         05  PROBAB-INCUMP-ABD07-WS        PIC 9(3)  .            ABB0009
RZN001         05  RAZON-BLOQUEO-ABD07-WS         PIC X(1).             CAV0007
NHDC01     03  VALOR-SALDO-DEUDAM-ABD07-WS   PIC 9(7)  .                ABB0009
NHDC01     03  VALOR-DISPONIBLEM-ABD07-WS    PIC 9(7)  .                ABB0009
NHDC01     03  VALOR-SALDO-MORAM-ABD07-WS    PIC 9(7)  .                ABB0009
NHDC01     03  CALIFICACIONM-ABD07-WS        PIC 9     .                ABB0009
NHDC01     03  TIPO-MONEDA-ABD07-WS          PIC 9     .                ABB0009
NHDC01     03  TIPO-OBLIGACION-ABD07-WS      PIC 9     .                ABB0009
NHDC01     03  TIPO-GARANTIA-ABD07-WS        PIC 9     .                ABB0009
NHDC01     03  CUOTAS-CANCELADAS-ABD07-WS    PIC 9(3)  .                ABB0009
NHDC01     03  FECHA-DE-PAGO-ABD07-WS        PIC 9(8)  .                ABB0009
NHDC01     03  TOTAL-CUOTAS-ABD07-WS         PIC 9(3)  .                ABB0009
NHDC01     03  CUOTAS-EN-MORA-ABD07-WS       PIC 9(3)  .                ABB0009
NHDC01     03  SITUACION-ABD07-WS            PIC X.                     ABB0009
NHDC01     03  FECHA-ACT-END-ABD07-WS        PIC 9(6)  .                ABB0009
NHDC01     03  CAMPOS-NUEVOS-NHC-ABD07-WS                               ABB0009
NHDC01                 REDEFINES FECHA-ACT-END-ABD07-WS.                ABB0009
NHDC01         05  ESTADO-ORIGEN-ABD07-WS        PIC 9(1)  .            ABB0009
NHDC01         05  CAMPOS-TARJETA-ABD07-WS.                             ABB0009
NHDC01             07 TARJ-MARCA-PRIV-ABD07-WS   PIC 9(3)  .            ABB0009
NHDC01             07 REAL-CLAUS-PERM-ABD07-WS   PIC 9(3)               ABB0009
NHDC01                REDEFINES TARJ-MARCA-PRIV-ABD07-WS.               ABB0009
NHDC01         05 SITUACION-TITULAR-ABD07-WS PIC 9(1).                  ABB0009
NHDC01     03  CONSECUTIVO-ABD07-WS          PIC 99    .                ABB0009
NHDC01     03  FECHA-MORA-MAXIMA-ABD07-WS    PIC 9(6)  .                ABB0009
NHDC01     03  FECHA-ACTUALIZ-VALOR-ABD07-WS PIC 9(6)  .                ABB0009
NHDC01     03  FECHA-SITUACION-ABD07-WS      PIC 9(6)  .                ABB0009
NHDC01     03  VECTOR-2-ABD07-WS             PIC 9(12) .                ABB0009
NHDC01     03  VECTOR-HIS-2-ABD07-WS         PIC 9(12)                  ABB0009
NHDC01         REDEFINES                  VECTOR-2-ABD07-WS.            ABB0009
NHDC01     03  PERIODICIDAD-PAGO-ABD07-WS    PIC 9     .                ABB0009
NHDC01     03  GARANTE-ABD07-WS              PIC 9(2)  .                ABB0009
NHDC01     03  FORMA-PAGO-TOTAL-ABD07-WS     PIC 9     .                ABB0009
NHDC01     03  MOROSIDAD-ABD07-WS            PIC 9(3)  .                ABB0009
NHDC01     03  NOV-REPORT2-ABD07-WS          PIC 9(2).                  ABB0009
NHDC01     03  VECTOR-2-R-ABD07-WS           PIC 9(12) .                ABB0009
NHDC01     03  VECTOR-HIS-4-ABD07-WS         PIC 9(12)                  ABB0009
NHDC01         REDEFINES                  VECTOR-2-R-ABD07-WS.          ABB0009
NHDC01*    REGISTRO MAESTRO VALORES  PARA TRABAJO  1117                 ABB0009
NHDC01 01  REG-ICMVLR-WS                         PIC X(1117).           ABB0009
NHDC01 01  REG-ICMVLR-WS-R REDEFINES REG-ICMVLR-WS.                     ABB0009
NHDC01     03 LLAVE-ICMVLR-WS.                                          ABB0009
NHDC01        07  TIPO-ID-ICMVLR-WS              PIC 9.                 ABB0009
NHDC01        07  NUM-ID-ICMVLR-WS               PIC 9(11).             ABB0009
NHDC01        07  COD-SUS-ICMVLR-WS              PIC 9(06).             ABB0009
NHDC01        07  COD-CTA-ICMVLR-WS              PIC 99.                ABB0009
NHDC01        07  NUM-CTA-ICMVLR-WS              PIC X(18).             ABB0009
NHDC01     03 VALORES-ICMVLR-WS            OCCURS  12  TIMES.           ABB0009
NHDC01        05  CUOTA-ICMVLR-WS                PIC 9(11)  .           ABB0009
NHDC01        05  VAL-DEBITOS-EFEC-ICMVLR-WS     PIC 9(11)              ABB0009
NHDC01                             REDEFINES CUOTA-ICMVLR-WS.           ABB0009
NHDC01        05  V-IND-CUOTA-ICMVLR-WS          PIC 9.                 ABB0009
NHDC01        05  V-IND-DEBITOS-EFEC-ICMVLR-WS   PIC 9                  ABB0009
NHDC01                             REDEFINES V-IND-CUOTA-ICMVLR-WS.     ABB0009
NHDC01        05  SALDO-ICMVLR-WS                PIC 9(11)  .           ABB0009
NHDC01        05  VAL-CREDITO-EFEC-ICMVLR-WS     PIC 9(11)              ABB0009
NHDC01                             REDEFINES SALDO-ICMVLR-WS.           ABB0009
NHDC01        05  V-IND-SALDO-ICMVLR-WS          PIC 9.                 ABB0009
NHDC01        05  V-IND-CREDITO-EFEC-ICMVLR-WS   PIC 9                  ABB0009
NHDC01                             REDEFINES V-IND-SALDO-ICMVLR-WS.     ABB0009
NHDC01        05  SALDO-MORA-ICMVLR-WS           PIC 9(11)  .           ABB0009
NHDC01        05  VAL-DEBITOS-TRANSF-ICMVLR-WS   PIC 9(11)              ABB0009
NHDC01                             REDEFINES SALDO-MORA-ICMVLR-WS.      ABB0009
NHDC01        05  V-IND-SALDO-MORA-ICMVLR-WS     PIC 9.                 ABB0009
NHDC01        05  V-IND-DEBITOS-TRANSF-ICMVLR-WS PIC 9                  ABB0009
NHDC01                             REDEFINES V-IND-SALDO-MORA-ICMVLR-WS.ABB0009
NHDC01        05  VAL-INICIAL-ICMVLR-WS          PIC 9(11)  .           ABB0009
NHDC01        05  VAL-CREDIT-TRANSF-ICMVLR-WS    PIC 9(11)              ABB0009
NHDC01                             REDEFINES VAL-INICIAL-ICMVLR-WS.     ABB0009
NHDC01        05  V-IND-VAL-INIC-ICMVLR-WS       PIC 9.                 ABB0009
NHDC01        05  V-IND-CREDIT-TRANSF-ICMVLR-WS  PIC 9                  ABB0009
NHDC01                             REDEFINES V-IND-VAL-INIC-ICMVLR-WS.  ABB0009
NHDC01        05  SALDO-DISP-ICMVLR-WS           PIC 9(11)  .           ABB0009
NHDC01        05  VAL-CHEQ-DEVUELT-ICMVLR-WS     PIC 9(11)              ABB0009
NHDC01                             REDEFINES SALDO-DISP-ICMVLR-WS.      ABB0009
NHDC01        05  V-IND-SALDO-DISP-ICMVLR-WS     PIC 9.                 ABB0009
NHDC01        05  V-IND-CHEQ-DEVUELT-ICMVLR-WS   PIC 9                  ABB0009
NHDC01                             REDEFINES V-IND-SALDO-DISP-ICMVLR-WS.ABB0009
NHDC01        05  MOROSIDAD-ICMVLR-WS            PIC 9(3) .             ABB0009
NHDC01        05  CANT-CHEQ-DEVU-ICMVLR-WS       PIC 9(3)               ABB0009
NHDC01                               REDEFINES MOROSIDAD-ICMVLR-WS.     ABB0009
NHDC01        05  V-IND-MOROSIDAD-ICMVLR-WS      PIC 9.                 ABB0009
NHDC01        05  V-IND-CANT-CHEQ-DEVU-ICMVLR-WS PIC 9                  ABB0009
NHDC01                             REDEFINES V-IND-MOROSIDAD-ICMVLR-WS. ABB0009
MICHCP        05  INF-CTES-Y-AHO-ICMVLR-WS.                             VHF0009
MICHCP            07  VAL-CHEQ-PAGADOS-ICMVLR-WS     PIC 9(11)  .       VHF0009
MICHCP            07  V-IND-CHEQ-PAGADOS-ICMVLR-WS   PIC 9.             VHF0009
MICHCP            07  CANT-CHEQ-PAG-ICMVLR-WS        PIC 9(3)   .       VHF0009
MICHCP            07  V-IND-CANT-CHEQ-PAG-ICMVLR-WS  PIC 9.             VHF0009
MICHCP        05  INF-CARTERA-ICMVLR-WS REDEFINES                       VHF0009
MICHCP                                  INF-CTES-Y-AHO-ICMVLR-WS.       VHF0009
MICHCP            07  FECHA-PAGO-ICMVLR-WS           PIC 9(06).         VHF0009
MICHCP            07  V-IND-FECHA-PAGO-ICMVLR-WS     PIC 9.             VHF0009
MICHCP            07  FECHA-LIM-PAGO-ICMVLR-WS       PIC 9(06).         VHF0009
MICHCP            07  V-IND-FECHA-LIM-PAGO-ICMVLR-WS PIC 9.             VHF0009
MICHCP            07  FILLER                         PIC X(02).         VHF0009
NHDC01     03 CAMPOS-UBICACION-CUENTA-VALVLR.                           ABB0009
NHDC01        05  CODIGO-DANE-ICMVLR-WS          PIC 9(8) .             ABB0009
NHDC01        05  CIUDAD-RADICACION-ICMVLR-WS    PIC X(30).             ABB0009
NHDC01        05  OFICINA-RADICACION-ICMVLR-WS   PIC X(30).             ABB0009
NHDC01     03 MIGRADO-UNIFICADO-ICMVLR-WS        PIC X(01).             ABB0009
MICHCP     03 VECTOR-CALIFIC-ICMVLR-WS           OCCURS 12 TIMES.       VHF0009
MICHCP        05  CALIFICACION-ICMVLR-WS         PIC X(01).             VHF0009
MICHCP        05  V-IND-CALIFICACION-ICMVLR-WS   PIC 9.                 VHF0009
MICHCP     03  FILLER                            PIC X(26).             VHF0009
NHDC01 01  REG-ICMVLR2-WS.                                              ABB0009
NHDC01     03 LLAVE-ICMVLR2-WS.                                         ABB0009
NHDC01        07  TIPO-ID-ICMVLR2-WS              PIC 9.                ABB0009
NHDC01        07  NUM-ID-ICMVLR2-WS               PIC 9(11).            ABB0009
NHDC01        07  COD-SUS-ICMVLR2-WS              PIC 9(06).            ABB0009
NHDC01        07  COD-CTA-ICMVLR2-WS              PIC 99.               ABB0009
NHDC01        07  NUM-CTA-ICMVLR2-WS              PIC X(18).            ABB0009
NHDC01     03 VALORES-ICMVLR2-WS            OCCURS  12  TIMES.          ABB0009
NHDC01        05  CUOTA-ICMVLR2-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-EFEC-ICMVLR2-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   CUOTA-ICMVLR2-WS.         ABB0009
NHDC01        05  V-IND-CUOTA-ICMVLR2-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-EFEC-ICMVLR2-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-CUOTA-ICMVLR2-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-ICMVLR2-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDITO-EFEC-ICMVLR2-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-ICMVLR2-WS.         ABB0009
NHDC01        05  V-IND-SALDO-ICMVLR2-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-CREDITO-EFEC-ICMVLR2-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-SALDO-ICMVLR2-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-MORA-ICMVLR2-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-TRANSF-ICMVLR2-WS   PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-MORA-ICMVLR2-WS.    ABB0009
NHDC01        05  V-IND-SALDO-MORA-ICMVLR2-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-TRANSF-ICMVLR2-WS PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-MORA-ICMVLR2-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  VAL-INICIAL-ICMVLR2-WS          PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDIT-TRANSF-ICMVLR2-WS    PIC 9(11)             ABB0009
NHDC01            REDEFINES                   VAL-INICIAL-ICMVLR2-WS.   ABB0009
NHDC01        05  V-IND-VAL-INIC-ICMVLR2-WS       PIC 9.                ABB0009
NHDC01        05  V-IND-CREDIT-TRANSF-ICMVLR2-WS  PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-VAL-INIC-ICMVLR2-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-DISP-ICMVLR2-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-CHEQ-DEVUELT-ICMVLR2-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-DISP-ICMVLR2-WS.    ABB0009
NHDC01        05  V-IND-SALDO-DISP-ICMVLR2-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-CHEQ-DEVUELT-ICMVLR2-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-DISP-ICMVLR2-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  MOROSIDAD-ICMVLR2-WS            PIC 9(3). *> MESES    ABB0009
NHDC01        05  CANT-CHEQ-DEVU-ICMVLR2-WS       PIC 9(3)              ABB0009
NHDC01            REDEFINES                   MOROSIDAD-ICMVLR2-WS.     ABB0009
NHDC01        05  V-IND-MOROSIDAD-ICMVLR2-WS      PIC 9.                ABB0009
NHDC01        05  V-IND-CANT-CHEQ-DEVU-ICMVLR2-WS PIC 9                 ABB0009
NHDC01            REDEFINES V-IND-MOROSIDAD-ICMVLR2-WS.                 ABB0009
MICHCP        05  INF-CTES-Y-AHO-ICMVLR2-WS.                            VHF0009
MICHCP            07  VAL-CHEQ-PAGADOS-ICMVLR2-WS     PIC 9(11)  .      VHF0009
MICHCP            07  V-IND-CHEQ-PAGADOS-ICMVLR2-WS   PIC 9.            VHF0009
MICHCP            07  CANT-CHEQ-PAG-ICMVLR2-WS        PIC 9(3)   .      VHF0009
MICHCP            07  V-IND-CANT-CHEQ-PAG-ICMVLR2-WS  PIC 9.            VHF0009
MICHCP        05  INF-CARTERA-ICMVLR2-WS REDEFINES                      VHF0009
MICHCP                                  INF-CTES-Y-AHO-ICMVLR2-WS.      VHF0009
MICHCP            07  FECHA-PAGO-ICMVLR2-WS           PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-PAGO-ICMVLR2-WS     PIC 9.            VHF0009
MICHCP            07  FECHA-LIM-PAGO-ICMVLR2-WS       PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-LIM-PAGO-ICMVLR2-WS PIC 9.            VHF0009
MICHCP            07  FILLER                         PIC X(02).         VHF0009
NHDC01     03 CAMPOS-UBICACIO-CTA-ICMVLR2-WS.                           ABB0009
NHDC01        05  CODIGO-DANE-ICMVLR2-WS          PIC 9(8).             ABB0009
NHDC01        05  CIUDAD-RADICACION-ICMVLR2-WS    PIC X(30).            ABB0009
NHDC01        05  OFICINA-RADICACION-ICMVLR2-WS   PIC X(30).            ABB0009
NHDC01     03 MIGRADO-UNIFICADO-ICMVLR2-WS        PIC X(01).            ABB0009
MICHCP     03 VECTOR-CALIFIC-ICMVLR2-WS           OCCURS 12 TIMES.      VHF0009
MICHCP        05  CALIFICACION-ICMVLR2-WS         PIC X(01).            VHF0009
MICHCP        05  V-IND-CALIFICACION-ICMVLR2-WS   PIC 9.                VHF0009
MICHCP     03  FILLER                             PIC X(26).            VHF0009
NHDC01 01  REG-ICMVLR3-WS.                                              ABB0009
NHDC01     03 LLAVE-ICMVLR3-WS.                                         ABB0009
NHDC01        07  TIPO-ID-ICMVLR3-WS              PIC 9.                ABB0009
NHDC01        07  NUM-ID-ICMVLR3-WS               PIC 9(11).            ABB0009
NHDC01        07  COD-SUS-ICMVLR3-WS              PIC 9(06).            ABB0009
NHDC01        07  COD-CTA-ICMVLR3-WS              PIC 99.               ABB0009
NHDC01        07  NUM-CTA-ICMVLR3-WS              PIC X(18).            ABB0009
NHDC01     03 VALORES-ICMVLR3-WS            OCCURS  12  TIMES.          ABB0009
NHDC01        05  CUOTA-ICMVLR3-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-EFEC-ICMVLR3-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   CUOTA-ICMVLR3-WS.         ABB0009
NHDC01        05  V-IND-CUOTA-ICMVLR3-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-EFEC-ICMVLR3-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-CUOTA-ICMVLR3-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-ICMVLR3-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDITO-EFEC-ICMVLR3-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-ICMVLR3-WS.         ABB0009
NHDC01        05  V-IND-SALDO-ICMVLR3-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-CREDITO-EFEC-ICMVLR3-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-SALDO-ICMVLR3-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-MORA-ICMVLR3-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-TRANSF-ICMVLR3-WS   PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-MORA-ICMVLR3-WS.    ABB0009
NHDC01        05  V-IND-SALDO-MORA-ICMVLR3-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-TRANSF-ICMVLR3-WS PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-MORA-ICMVLR3-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  VAL-INICIAL-ICMVLR3-WS          PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDIT-TRANSF-ICMVLR3-WS    PIC 9(11)             ABB0009
NHDC01            REDEFINES                   VAL-INICIAL-ICMVLR3-WS.   ABB0009
NHDC01        05  V-IND-VAL-INIC-ICMVLR3-WS       PIC 9.                ABB0009
NHDC01        05  V-IND-CREDIT-TRANSF-ICMVLR3-WS  PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-VAL-INIC-ICMVLR3-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-DISP-ICMVLR3-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-CHEQ-DEVUELT-ICMVLR3-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-DISP-ICMVLR3-WS.    ABB0009
NHDC01        05  V-IND-SALDO-DISP-ICMVLR3-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-CHEQ-DEVUELT-ICMVLR3-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-DISP-ICMVLR3-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  MOROSIDAD-ICMVLR3-WS            PIC 9(3). *> MESES    ABB0009
NHDC01        05  CANT-CHEQ-DEVU-ICMVLR3-WS       PIC 9(3)              ABB0009
NHDC01            REDEFINES                   MOROSIDAD-ICMVLR3-WS.     ABB0009
NHDC01        05  V-IND-MOROSIDAD-ICMVLR3-WS      PIC 9.                ABB0009
NHDC01        05  V-IND-CANT-CHEQ-DEVU-ICMVLR3-WS PIC 9                 ABB0009
NHDC01            REDEFINES V-IND-MOROSIDAD-ICMVLR3-WS.                 ABB0009
MICHCP        05  INF-CTES-Y-AHO-ICMVLR3-WS.                            VHF0009
MICHCP            07  VAL-CHEQ-PAGADOS-ICMVLR3-WS     PIC 9(11)  .      VHF0009
MICHCP            07  V-IND-CHEQ-PAGADOS-ICMVLR3-WS   PIC 9.            VHF0009
MICHCP            07  CANT-CHEQ-PAG-ICMVLR3-WS        PIC 9(3)   .      VHF0009
MICHCP            07  V-IND-CANT-CHEQ-PAG-ICMVLR3-WS  PIC 9.            VHF0009
MICHCP        05  INF-CARTERA-ICMVLR3-WS REDEFINES                      VHF0009
MICHCP                                  INF-CTES-Y-AHO-ICMVLR3-WS.      VHF0009
MICHCP            07  FECHA-PAGO-ICMVLR3-WS           PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-PAGO-ICMVLR3-WS     PIC 9.            VHF0009
MICHCP            07  FECHA-LIM-PAGO-ICMVLR3-WS       PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-LIM-PAGO-ICMVLR3-WS PIC 9.            VHF0009
MICHCP            07  FILLER                         PIC X(02).         VHF0009
NHDC01     03 CAMPOS-UBICACIO-CTA-ICMVLR3-WS.                           ABB0009
NHDC01        05  CODIGO-DANE-ICMVLR3-WS          PIC 9(8).             ABB0009
NHDC01        05  CIUDAD-RADICACION-ICMVLR3-WS    PIC X(30).            ABB0009
NHDC01        05  OFICINA-RADICACION-ICMVLR3-WS   PIC X(30).            ABB0009
NHDC01     03 MIGRADO-UNIFICADO-ICMVLR3-WS        PIC X(01).            ABB0009
MICHCP     03 VECTOR-CALIFIC-ICMVLR3-WS           OCCURS 12 TIMES.      VHF0009
MICHCP        05  CALIFICACION-ICMVLR3-WS         PIC X(01).            VHF0009
MICHCP        05  V-IND-CALIFICACION-ICMVLR3-WS   PIC 9.                VHF0009
MICHCP     03  FILLER                             PIC X(26).            VHF0009
NHDC01 01  REG-ICMVLR4-WS.                                              ABB0009
NHDC01     03 LLAVE-ICMVLR4-WS.                                         ABB0009
NHDC01        07  TIPO-ID-ICMVLR4-WS              PIC 9.                ABB0009
NHDC01        07  NUM-ID-ICMVLR4-WS               PIC 9(11).            ABB0009
NHDC01        07  COD-SUS-ICMVLR4-WS              PIC 9(06).            ABB0009
NHDC01        07  COD-CTA-ICMVLR4-WS              PIC 99.               ABB0009
NHDC01        07  NUM-CTA-ICMVLR4-WS              PIC X(18).            ABB0009
NHDC01     03 VALORES-ICMVLR4-WS            OCCURS  12  TIMES.          ABB0009
NHDC01        05  CUOTA-ICMVLR4-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-EFEC-ICMVLR4-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   CUOTA-ICMVLR4-WS.         ABB0009
NHDC01        05  V-IND-CUOTA-ICMVLR4-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-EFEC-ICMVLR4-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-CUOTA-ICMVLR4-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-ICMVLR4-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDITO-EFEC-ICMVLR4-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-ICMVLR4-WS.         ABB0009
NHDC01        05  V-IND-SALDO-ICMVLR4-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-CREDITO-EFEC-ICMVLR4-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-SALDO-ICMVLR4-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-MORA-ICMVLR4-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-TRANSF-ICMVLR4-WS   PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-MORA-ICMVLR4-WS.    ABB0009
NHDC01        05  V-IND-SALDO-MORA-ICMVLR4-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-TRANSF-ICMVLR4-WS PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-MORA-ICMVLR4-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  VAL-INICIAL-ICMVLR4-WS          PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDIT-TRANSF-ICMVLR4-WS    PIC 9(11)             ABB0009
NHDC01            REDEFINES                   VAL-INICIAL-ICMVLR4-WS.   ABB0009
NHDC01        05  V-IND-VAL-INIC-ICMVLR4-WS       PIC 9.                ABB0009
NHDC01        05  V-IND-CREDIT-TRANSF-ICMVLR4-WS  PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-VAL-INIC-ICMVLR4-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-DISP-ICMVLR4-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-CHEQ-DEVUELT-ICMVLR4-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-DISP-ICMVLR4-WS.    ABB0009
NHDC01        05  V-IND-SALDO-DISP-ICMVLR4-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-CHEQ-DEVUELT-ICMVLR4-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-DISP-ICMVLR4-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  MOROSIDAD-ICMVLR4-WS            PIC 9(3). *> MESES    ABB0009
NHDC01        05  CANT-CHEQ-DEVU-ICMVLR4-WS       PIC 9(3)              ABB0009
NHDC01            REDEFINES                   MOROSIDAD-ICMVLR4-WS.     ABB0009
NHDC01        05  V-IND-MOROSIDAD-ICMVLR4-WS      PIC 9.                ABB0009
NHDC01        05  V-IND-CANT-CHEQ-DEVU-ICMVLR4-WS PIC 9                 ABB0009
NHDC01            REDEFINES V-IND-MOROSIDAD-ICMVLR4-WS.                 ABB0009
MICHCP        05  INF-CTES-Y-AHO-ICMVLR4-WS.                            VHF0009
MICHCP            07  VAL-CHEQ-PAGADOS-ICMVLR4-WS     PIC 9(11)  .      VHF0009
MICHCP            07  V-IND-CHEQ-PAGADOS-ICMVLR4-WS   PIC 9.            VHF0009
MICHCP            07  CANT-CHEQ-PAG-ICMVLR4-WS        PIC 9(3)   .      VHF0009
MICHCP            07  V-IND-CANT-CHEQ-PAG-ICMVLR4-WS  PIC 9.            VHF0009
MICHCP        05  INF-CARTERA-ICMVLR4-WS REDEFINES                      VHF0009
MICHCP                                  INF-CTES-Y-AHO-ICMVLR4-WS.      VHF0009
MICHCP            07  FECHA-PAGO-ICMVLR4-WS           PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-PAGO-ICMVLR4-WS     PIC 9.            VHF0009
MICHCP            07  FECHA-LIM-PAGO-ICMVLR4-WS       PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-LIM-PAGO-ICMVLR4-WS PIC 9.            VHF0009
MICHCP            07  FILLER                         PIC X(02).         VHF0009
NHDC01     03 CAMPOS-UBICACIO-CTA-ICMVLR4-WS.                           ABB0009
NHDC01        05  CODIGO-DANE-ICMVLR4-WS          PIC 9(8).             ABB0009
NHDC01        05  CIUDAD-RADICACION-ICMVLR4-WS    PIC X(30).            ABB0009
NHDC01        05  OFICINA-RADICACION-ICMVLR4-WS   PIC X(30).            ABB0009
NHDC01     03 MIGRADO-UNIFICADO-ICMVLR4-WS        PIC X(01).            ABB0009
MICHCP     03 VECTOR-CALIFIC-ICMVLR4-WS           OCCURS 12 TIMES.      VHF0009
MICHCP        05  CALIFICACION-ICMVLR4-WS         PIC X(01).            VHF0009
MICHCP        05  V-IND-CALIFICACION-ICMVLR4-WS   PIC 9.                VHF0009
MICHCP     03  FILLER                             PIC X(26).            VHF0009
NHDC01 01  REG-ICMVLR5-WS.                                              ABB0009
NHDC01     03 LLAVE-ICMVLR5-WS.                                         ABB0009
NHDC01        07  TIPO-ID-ICMVLR5-WS              PIC 9.                ABB0009
NHDC01        07  NUM-ID-ICMVLR5-WS               PIC 9(11).            ABB0009
NHDC01        07  COD-SUS-ICMVLR5-WS              PIC 9(06).            ABB0009
NHDC01        07  COD-CTA-ICMVLR5-WS              PIC 99.               ABB0009
NHDC01        07  NUM-CTA-ICMVLR5-WS              PIC X(18).            ABB0009
NHDC01     03 VALORES-ICMVLR5-WS            OCCURS  12  TIMES.          ABB0009
NHDC01        05  CUOTA-ICMVLR5-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-EFEC-ICMVLR5-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   CUOTA-ICMVLR5-WS.         ABB0009
NHDC01        05  V-IND-CUOTA-ICMVLR5-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-EFEC-ICMVLR5-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-CUOTA-ICMVLR5-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-ICMVLR5-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDITO-EFEC-ICMVLR5-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-ICMVLR5-WS.         ABB0009
NHDC01        05  V-IND-SALDO-ICMVLR5-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-CREDITO-EFEC-ICMVLR5-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-SALDO-ICMVLR5-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-MORA-ICMVLR5-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-TRANSF-ICMVLR5-WS   PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-MORA-ICMVLR5-WS.    ABB0009
NHDC01        05  V-IND-SALDO-MORA-ICMVLR5-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-TRANSF-ICMVLR5-WS PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-MORA-ICMVLR5-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  VAL-INICIAL-ICMVLR5-WS          PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDIT-TRANSF-ICMVLR5-WS    PIC 9(11)             ABB0009
NHDC01            REDEFINES                   VAL-INICIAL-ICMVLR5-WS.   ABB0009
NHDC01        05  V-IND-VAL-INIC-ICMVLR5-WS       PIC 9.                ABB0009
NHDC01        05  V-IND-CREDIT-TRANSF-ICMVLR5-WS  PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-VAL-INIC-ICMVLR5-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-DISP-ICMVLR5-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-CHEQ-DEVUELT-ICMVLR5-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-DISP-ICMVLR5-WS.    ABB0009
NHDC01        05  V-IND-SALDO-DISP-ICMVLR5-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-CHEQ-DEVUELT-ICMVLR5-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-DISP-ICMVLR5-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  MOROSIDAD-ICMVLR5-WS            PIC 9(3). *> MESES    ABB0009
NHDC01        05  CANT-CHEQ-DEVU-ICMVLR5-WS       PIC 9(3)              ABB0009
NHDC01            REDEFINES                   MOROSIDAD-ICMVLR5-WS.     ABB0009
NHDC01        05  V-IND-MOROSIDAD-ICMVLR5-WS      PIC 9.                ABB0009
NHDC01        05  V-IND-CANT-CHEQ-DEVU-ICMVLR5-WS PIC 9                 ABB0009
NHDC01            REDEFINES V-IND-MOROSIDAD-ICMVLR5-WS.                 ABB0009
MICHCP        05  INF-CTES-Y-AHO-ICMVLR5-WS.                            VHF0009
MICHCP            07  VAL-CHEQ-PAGADOS-ICMVLR5-WS     PIC 9(11)  .      VHF0009
MICHCP            07  V-IND-CHEQ-PAGADOS-ICMVLR5-WS   PIC 9.            VHF0009
MICHCP            07  CANT-CHEQ-PAG-ICMVLR5-WS        PIC 9(3)   .      VHF0009
MICHCP            07  V-IND-CANT-CHEQ-PAG-ICMVLR5-WS  PIC 9.            VHF0009
MICHCP        05  INF-CARTERA-ICMVLR5-WS REDEFINES                      VHF0009
MICHCP                                  INF-CTES-Y-AHO-ICMVLR5-WS.      VHF0009
MICHCP            07  FECHA-PAGO-ICMVLR5-WS           PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-PAGO-ICMVLR5-WS     PIC 9.            VHF0009
MICHCP            07  FECHA-LIM-PAGO-ICMVLR5-WS       PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-LIM-PAGO-ICMVLR5-WS PIC 9.            VHF0009
MICHCP            07  FILLER                         PIC X(02).         VHF0009
NHDC01     03 CAMPOS-UBICACIO-CTA-ICMVLR5-WS.                           ABB0009
NHDC01        05  CODIGO-DANE-ICMVLR5-WS          PIC 9(8).             ABB0009
NHDC01        05  CIUDAD-RADICACION-ICMVLR5-WS    PIC X(30).            ABB0009
NHDC01        05  OFICINA-RADICACION-ICMVLR5-WS   PIC X(30).            ABB0009
NHDC01     03 MIGRADO-UNIFICADO-ICMVLR5-WS        PIC X(01).            ABB0009
MICHCP     03 VECTOR-CALIFIC-ICMVLR5-WS           OCCURS 12 TIMES.      VHF0009
MICHCP        05  CALIFICACION-ICMVLR5-WS         PIC X(01).            VHF0009
MICHCP        05  V-IND-CALIFICACION-ICMVLR5-WS   PIC 9.                VHF0009
MICHCP     03  FILLER                             PIC X(26).            VHF0009
NHDC01 01  REG-ICMVLR6-WS.                                              ABB0009
NHDC01     03 LLAVE-ICMVLR6-WS.                                         ABB0009
NHDC01        07  TIPO-ID-ICMVLR6-WS              PIC 9.                ABB0009
NHDC01        07  NUM-ID-ICMVLR6-WS               PIC 9(11).            ABB0009
NHDC01        07  COD-SUS-ICMVLR6-WS              PIC 9(06).            ABB0009
NHDC01        07  COD-CTA-ICMVLR6-WS              PIC 99.               ABB0009
NHDC01        07  NUM-CTA-ICMVLR6-WS              PIC X(18).            ABB0009
NHDC01     03 VALORES-ICMVLR6-WS            OCCURS  12  TIMES.          ABB0009
NHDC01        05  CUOTA-ICMVLR6-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-EFEC-ICMVLR6-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   CUOTA-ICMVLR6-WS.         ABB0009
NHDC01        05  V-IND-CUOTA-ICMVLR6-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-EFEC-ICMVLR6-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-CUOTA-ICMVLR6-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-ICMVLR6-WS                PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDITO-EFEC-ICMVLR6-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-ICMVLR6-WS.         ABB0009
NHDC01        05  V-IND-SALDO-ICMVLR6-WS          PIC 9.                ABB0009
NHDC01        05  V-IND-CREDITO-EFEC-ICMVLR6-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-SALDO-ICMVLR6-WS.   ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-MORA-ICMVLR6-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-DEBITOS-TRANSF-ICMVLR6-WS   PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-MORA-ICMVLR6-WS.    ABB0009
NHDC01        05  V-IND-SALDO-MORA-ICMVLR6-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-DEBITOS-TRANSF-ICMVLR6-WS PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-MORA-ICMVLR6-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  VAL-INICIAL-ICMVLR6-WS          PIC 9(11).            ABB0009
NHDC01        05  VAL-CREDIT-TRANSF-ICMVLR6-WS    PIC 9(11)             ABB0009
NHDC01            REDEFINES                   VAL-INICIAL-ICMVLR6-WS.   ABB0009
NHDC01        05  V-IND-VAL-INIC-ICMVLR6-WS       PIC 9.                ABB0009
NHDC01        05  V-IND-CREDIT-TRANSF-ICMVLR6-WS  PIC 9                 ABB0009
NHDC01            REDEFINES                   V-IND-VAL-INIC-ICMVLR6-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  SALDO-DISP-ICMVLR6-WS           PIC 9(11).            ABB0009
NHDC01        05  VAL-CHEQ-DEVUELT-ICMVLR6-WS     PIC 9(11)             ABB0009
NHDC01            REDEFINES                   SALDO-DISP-ICMVLR6-WS.    ABB0009
NHDC01        05  V-IND-SALDO-DISP-ICMVLR6-WS     PIC 9.                ABB0009
NHDC01        05  V-IND-CHEQ-DEVUELT-ICMVLR6-WS   PIC 9                 ABB0009
NHDC01            REDEFINES                 V-IND-SALDO-DISP-ICMVLR6-WS.ABB0009
NHDC01                                                                  ABB0009
NHDC01        05  MOROSIDAD-ICMVLR6-WS            PIC 9(3). *> MESES    ABB0009
NHDC01        05  CANT-CHEQ-DEVU-ICMVLR6-WS       PIC 9(3)              ABB0009
NHDC01            REDEFINES                   MOROSIDAD-ICMVLR6-WS.     ABB0009
NHDC01        05  V-IND-MOROSIDAD-ICMVLR6-WS      PIC 9.                ABB0009
NHDC01        05  V-IND-CANT-CHEQ-DEVU-ICMVLR6-WS PIC 9                 ABB0009
NHDC01            REDEFINES V-IND-MOROSIDAD-ICMVLR6-WS.                 ABB0009
MICHCP        05  INF-CTES-Y-AHO-ICMVLR6-WS.                            VHF0009
MICHCP            07  VAL-CHEQ-PAGADOS-ICMVLR6-WS     PIC 9(11)  .      VHF0009
MICHCP            07  V-IND-CHEQ-PAGADOS-ICMVLR6-WS   PIC 9.            VHF0009
MICHCP            07  CANT-CHEQ-PAG-ICMVLR6-WS        PIC 9(3)   .      VHF0009
MICHCP            07  V-IND-CANT-CHEQ-PAG-ICMVLR6-WS  PIC 9.            VHF0009
MICHCP        05  INF-CARTERA-ICMVLR6-WS REDEFINES                      VHF0009
MICHCP                                  INF-CTES-Y-AHO-ICMVLR6-WS.      VHF0009
MICHCP            07  FECHA-PAGO-ICMVLR6-WS           PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-PAGO-ICMVLR6-WS     PIC 9.            VHF0009
MICHCP            07  FECHA-LIM-PAGO-ICMVLR6-WS       PIC 9(06).        VHF0009
MICHCP            07  V-IND-FECHA-LIM-PAGO-ICMVLR6-WS PIC 9.            VHF0009
MICHCP            07  FILLER                         PIC X(02).         VHF0009
NHDC01     03 CAMPOS-UBICACIO-CTA-ICMVLR6-WS.                           ABB0009
NHDC01        05  CODIGO-DANE-ICMVLR6-WS          PIC 9(8).             ABB0009
NHDC01        05  CIUDAD-RADICACION-ICMVLR6-WS    PIC X(30).            ABB0009
NHDC01        05  OFICINA-RADICACION-ICMVLR6-WS   PIC X(30).            ABB0009
NHDC01     03 MIGRADO-UNIFICADO-ICMVLR6-WS        PIC X(01).            ABB0009
MICHCP     03 VECTOR-CALIFIC-ICMVLR6-WS           OCCURS 12 TIMES.      VHF0009
MICHCP        05  CALIFICACION-ICMVLR6-WS         PIC X(01).            VHF0009
MICHCP        05  V-IND-CALIFICACION-ICMVLR6-WS   PIC 9.                VHF0009
MICHCP     03  FILLER                             PIC X(26).            VHF0009
      *   REGISTRO MAESTRO ENDEUDAMIENTO PARA TRABAJO
       01 MSUP-REGISTRO-WS.
          03 MSUP-LLAVE-WS.
             05 MSUP-TIP-ID-WS        PIC 9(1).
             05 MSUP-NUM-ID-WS        PIC 9(11).
             05 MSUP-FECHA-WS         PIC 9(8).
             05 MSUP-TIP-ENTID-WS     PIC 9(2).
             05 MSUP-COD-ENTID-WS     PIC 9(6).
             05 MSUP-MONEDA-WS        PIC X(1).
             05 MSUP-TIP-DEUDA-WS     PIC X(1).
             05 MSUP-CALIFICA-WS      PIC X(1).
             05 MSUP-GARANTIA-WS      PIC X(1).
          03 MSUP-ITEMS-WS.
             05 MSUP-SIGVAL-WS        PIC X(01).
             05 MSUP-VALOR-WS         PIC 9(14).
             05 MSUP-SIGINT-WS        PIC X(01).
             05 MSUP-INTERES-WS       PIC 9(14).
             05 MSUP-SIGCON-WS        PIC X(01).
             05 MSUP-CONTING-WS       PIC 9(14).
             05 MSUP-CANTIDAD-WS      PIC 9(3).
             05 MSUP-IND-BLOQ-WS      PIC 9(1).
             05 MSUP-ART-15-WS        PIC 9(2).
             05 MSUP-FEC-ART-15-WS    PIC 9(8).
             05 MSUP-FEC-ACTUALIZ-WS  PIC 9(8).
             05 MSUP-ART-15-CTI-WS    PIC 9(2).
             05 MSUP-FEC-ART-15-CTI-WS
                                      PIC 9(8).
             05 MSUP-NUM-RECLAMO-WS   PIC 9(5).
             05 FILLER                PIC X(13).
