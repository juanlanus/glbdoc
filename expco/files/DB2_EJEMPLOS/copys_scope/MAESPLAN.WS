REMARK*** MAESPLAN-WS.v.0008
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
024800*----------------------------------------------------------------*
       01 CTA-CTE                     PIC 9(02)  VALUE 2.
       01 CARTERA                     PIC 9(02)  VALUE 3.
       01 ENDEUDAMIENTO               PIC 9(02)  VALUE 10.
       01 TIP-CTA-RECLAMO-WS          PIC 9(02).
       01 REG-MAESTR-PLANO            PIC X(400).                       VHF0003
       01 MAPA-BITS-WS.                                                 VHF0002
          03 MAP-BIT-WS               PIC 9(01)  OCCURS 15 TIMES.       VHF0002
       01 I-MAESPLAN                  PIC 9(05).                        VHF0002

      *    REGISTRO MAESTRO CUENTAS CORRIENTES PARA TRABAJO
       01  COMPLETO-ABD06-WS.
           03  LLAVE-MAES-CHECK-WS.
               05  LLAVE-MAES-CHECK-PRI-WS.
                   07  TIPO-ID-ABD06-WS
                                      PIC 9.
                   07  NUM-ID-ABD06-WS
                                      PIC 9(11).
               05  LLAVE-MAES-CHECK-ALT-WS.
                   07  COD-SUS-ABD06-WS
                                      PIC 9(6).
                   07  COD-CTA-ABD06-WS
                                      PIC 99.
                   07  NUM-CTA-ABD06-WS
                                      PIC X(18).
           03  IND-BLOQUEO-ABD06-WS   PIC 9.
           03  FECHA-APERT-ABD06-WS   PIC 9(4).
           03  CALIFIC-ABD06-WS       PIC 9.
           03  TIP-CALIFIC-ABD06-WS   PIC 9.
           03  NOV-REPORT-ABD06-WS    PIC 99.
           03  FEC-NOVEDAD-ABD06-WS   PIC 9(4).
           03  NUM-EMBARGO-ABD06-WS   PIC 99.
           03  FEC-ULT-EMB-ABD06-WS   PIC 9(4).
           03  ADJETIVO-ABD06-WS      PIC 9.
           03  FEC-ADJETIVO-ABD06-WS  PIC 9(4).
           03  IND-MODIFICACION-ABD06-WS  PIC 9(01).                    VHF0004
           03  IND-RECLAMO-ABD06-WS   PIC 9(01).                        VHF0004
REGLOC     03  REG-LOCK-ABD06-WS      PIC 9(01).                        FAC0005 
REGLOC     03  FILLER-RECLAMO-ABD06-WS    PIC 9(01).                    FAC0005 
           03  CONSECUTIVO-ABD06-WS   PIC 99.
           03  OFICINA-ABD06-WS       PIC X(15).
           03  CIUDAD-ABD06-WS        PIC X(6).
           03  FEC-NOVANT-ABD06-WS    PIC 9(4).                         VHF0002
           03  NOVANT-ABD06-WS        PIC 9(2).                         VHF0002
LEY007     03  IND-SOBREGIRO-ABD06-WS PIC 9(01).                        VHF0006
LEY007     03  COD-EST-TITUL-ABD06-WS PIC 9(01).                        VHF0006
LEY007     03  CALIFICACION-ABD06-WS  PIC 9(01).                        VHF0006
RZN001*    03  FILLER-ABD06-WS        PIC X(01).                        CAV0007
RZN001     03  RAZON-BLOQUEO-ABD06-WS PIC X(01).                        CAV0007

      *    REGISTRO MAESTRO CARTERA PARA TRABAJO
       01  COMPLETO-ABD07-WS.
           03  LLAVE-MAES-CREDIT-WS.
               05  LLAVE-MAES-CREDIT-PRI-WS.
                   07  TIPO-ID-ABD07-WS
                                      PIC 9.
                   07  NUM-ID-ABD07-WS
                                      PIC 9(11).
               05  LLAVE-MAES-CREDIT-ALT-WS.
                   07  COD-SUS-ABD07-WS
                                      PIC 9(6).
                   07  COD-CTA-ABD07-WS
                                      PIC 99.
                   07  NUM-CTA-ABD07-WS
                                      PIC X(18).
                   07  NUM-CTA-VEN16-ABD07-WS
                                      REDEFINES  NUM-CTA-ABD07-WS.
                       09  NUM-CTA-VEN-ABD07-WS
                                      PIC 9(16).
                       09  GARANTE-VEN-ABD07-WS
                                      PIC 99.
           03  IND-BLOQUEO-ABD07-WS   PIC 9.
           03  FECHA-APERT-ABD07-WS   PIC 9(4).
           03  FECHA-VENC-ABD07-WS    PIC 9(4).
           03  CUANTIA-ABD07-WS       PIC 9(5).
           03  CALIFIC-ABD07-WS       PIC 9.
           03  TIP-CALIFIC-ABD07-WS   PIC 9.
           03  NOV-REPORT-ABD07-WS    PIC 99.
           03  FEC-NOVEDAD-ABD07-WS   PIC 9(4).
           03  NUM-MORAS30-ABD07-WS   PIC 99.
           03  ESTADO-MES-ABD07-WS    PIC 9(12).
           03  MORA-MAXIMA-ABD07-WS   PIC 9.
           03  ADJETIVO1-ABD07-WS     PIC 9.
           03  FEC-MEN-ADJ-ABD07-WS   PIC 9(4).
           03  ADJETIVO2-ABD07-WS     PIC 9.
           03  FEC-MAY-ADJ-ABD07-WS   PIC 9(4).
           03  ADJETIVO3-ABD07-WS     PIC 9.
LEY007     03  ART15-ABD07-WS         PIC 9(01).                        VHF0006
LEY007     03  IND-MODIFICACION-ABD07-WS  PIC 9(01)                     VHF0006
LEY007         REDEFINES ART15-ABD07-WS.                                VHF0006
LEY007     03 IND-RECLAMO-ABD07-WS PIC 9(01).                           VHF0006
LEY007     03 REG-LOCK-ABD07-WS    PIC 9(01).                           VHF0006
LEY007     03 FILLER-RECLAMO-ABD07-WS  PIC 9(01).                       VHF0006
LEY007     03 TARJ-ESTADO-PLAST-ABD07-WS   PIC 9(1)  REDEFINES          VHF0006
LEY007        FILLER-RECLAMO-ABD07-WS.                                  VHF0006
LEY007     03 REAL-TERMINO-CONTRATO-ABD07-WS PIC 9(1)                   VHF0006
LEY007        REDEFINES FILLER-RECLAMO-ABD07-WS.                        VHF0006
LEY007     03 IND-SUBSIDIO-HIPOTEC-ABD07-WS  PIC 9(1)                   VHF0006
LEY007        REDEFINES FILLER-RECLAMO-ABD07-WS.                        VHF0006
MICROC*    03  IND-BLOQUEO-R-ABD07-WS PIC 9.                            CAV0008
MICROC     03  IND-BLOQUEO-R-ABD07-WS PIC X.                            CAV0008
MICROC     03  TIPO-GAR-DETALLE-ABD07-WS PIC X   REDEFINES              CAV0008
MICROC                                IND-BLOQUEO-R-ABD07-WS.           CAV0008
           03  CALIFIC-R-ABD07-WS     PIC 9.
           03  TIP-CALIFIC-R-ABD07-WS PIC 9.
           03  IND-MAPA-BITS-VLR-ABD07-WS                               VHF0002
                                      REDEFINES TIP-CALIFIC-R-ABD07-WS  VHF0002
                                      PIC 9.                            VHF0002
           03  NOV-REPORT-R-ABD07-WS  PIC 99.
           03  FEC-NOVEDAD-R-ABD07-WS PIC 9(4).
           03  MAPA-BITS-VLR-ABD07-WS PIC 9(15).                        VHF0003
           03  ESTADO-MES-R-ABD07-WS  PIC 9(12).
           03  MORA-MAXIMA-R-ABD07-WS PIC 9.
           03  VALOR-INICIAL-ABD07-WS PIC 9(7).
LEY007*    03  VALOR-SALDO-DEUDA-ABD07-WS-R      OCCURS  4 TIMES.       VHF0006
LEY007*        05  VALOR-SALDO-DEUDA-ABD07-WS                           VHF0006
LEY007*                               PIC  9(7).                        VHF0006
LEY007*    03  VALOR-DISPONIBLE-ABD07-WS-R       OCCURS 4 TIMES.        VHF0006
LEY007*        05  VALOR-DISPONIBLE-ABD07-WS                            VHF0006
LEY007*                               PIC  9(7).                        VHF0006
LEY007*    03  VALOR-SALDO-MORA-ABD07-WS-R       OCCURS 4 TIMES.        VHF0006
LEY007*        05  VALOR-SALDO-MORA-ABD07-WS                            VHF0006
LEY007*                               PIC  9(7).                        VHF0006
LEY007*    03  CALIFICACION-ABD07-WS-R           OCCURS 4 TIMES.        VHF0006
LEY007*        05  CALIFICACION-ABD07-WS                                VHF0006
LEY007*                               PIC  9.                           VHF0006
LEY007     03  NUEVOS-CAMPOS-HISTORIA-CRE-WS .                          VHF0006
LEY007        05  FECHA-APERTURA-ABD07-WS        PIC 9(8).              VHF0006
LEY007        05  FECHA-VENCIMIENTO-ABD07-WS     PIC 9(8).              VHF0006
LEY007        05  FECHA-ADJETIVO1-ABD07-WS       PIC 9(8).              VHF0006
LEY007        05  FECHA-ADJETIVO2-ABD07-WS       PIC 9(8).              VHF0006
LEY007        05  FECHA-ADJETIVO3-ABD07-WS       PIC 9(8).              VHF0006
LEY007* DEFINICIONES DE NUEVOS CAMPOS                                   VHF0006
LEY007         05  ESTADO-OBLIG-ABD07-WS          PIC 9(2).             VHF0006
LEY007         05  FECHA-ESTADO-OBLIG-ABD07-WS    PIC 9(8).             VHF0006
LEY007         05  FECHA-ESTADO-ORIGEN-ABD07-WS   PIC 9(8).             VHF0006
LEY007         05  FECHA-LIMITE-PAGO-ABD07-WS     PIC 9(8).             VHF0006
LEY007         05  TARJ-FEC-ESTADO-PLAST-ABD07-WS  PIC 9(8).            VHF0006
LEY007         05  REAL-FEC-CLAUS-PERM-ABD07-WS   PIC 9(8)              VHF0006
LEY007             REDEFINES TARJ-FEC-ESTADO-PLAST-ABD07-WS .           VHF0006
LEY007         05  FEC-SUBSIDIO-HIPOTEC-ABD07-WS  PIC 9(8)              VHF0006
LEY007             REDEFINES TARJ-FEC-ESTADO-PLAST-ABD07-WS .           VHF0006
LEY007         05  TARJ-CLASE-ABD07-WS            PIC 9(1).             VHF0006
LEY007         05  TARJ-FRANQUICIA-ABD07-WS       PIC 9(1).             VHF0006
LEY007         05  PROBAB-INCUMP-ABD07-WS         PIC 9(3).             VHF0006
RZN001         05  RAZON-BLOQUEO-ABD07-WS         PIC X(1).             CAV0007
RZN002         05  FILLER                         PIC 9(8).             VHF0008
LEY007* CONTINUAN LOS CAMPOS QUE YA EXISTIAN ANTES                      VHF0006
           03  VALOR-SALDO-DEUDAM-ABD07-WS
                                      PIC 9(7).
           03  VALOR-DISPONIBLEM-ABD07-WS
                                      PIC 9(7).
           03  VALOR-SALDO-MORAM-ABD07-WS
                                      PIC 9(7).
           03  CALIFICACIONM-ABD07-WS PIC 9.
           03  TIPO-MONEDA-ABD07-WS   PIC 9.
           03  TIPO-OBLIGACION-ABD07-WS
                                      PIC 9.
           03  TIPO-GARANTIA-ABD07-WS PIC 9.
           03  CUOTAS-CANCELADAS-ABD07-WS
                                      PIC 9(3).
           03  FECHA-DE-PAGO-ABD07-WS PIC 9(8).
           03  TOTAL-CUOTAS-ABD07-WS  PIC 9(3).
           03  CUOTAS-EN-MORA-ABD07-WS
                                      PIC 9(3).
           03  SITUACION-ABD07-WS     PIC X.
           03  FECHA-ACT-END-ABD07-WS PIC 9(6).
LEY007     03  CAMPOS-NUEVOS-NHC-ABD07-WS  REDEFINES                    VHF0006
LEY007         FECHA-ACT-END-ABD07-WS .                                 VHF0006
LEY007         05  ESTADO-ORIGEN-ABD07-WS         PIC 9(1).             VHF0006
LEY007         05  CAMPOS-TARJETA-ABD07-WS .                            VHF0006
LEY007             07 TARJ-MARCA-PRIV-ABD07-WS    PIC 9(3).             VHF0006
LEY007             07 REAL-CLAUS-PERM-ABD07-WS    PIC 9(3)              VHF0006
LEY007                REDEFINES TARJ-MARCA-PRIV-ABD07-WS .              VHF0006
LEY007         05 SITUACION-TITULAR-ABD07-WS  PIC 9(1).                 VHF0006
           03  CONSECUTIVO-ABD07-WS   PIC 99.
           03  FECHA-MORA-MAXIMA-ABD07-WS
                                      PIC 9(6).
           03  FECHA-ACTUALIZ-VALOR-ABD07-WS
                                      PIC 9(6).
           03  FECHA-SITUACION-ABD07-WS
                                      PIC 9(6).
           03  VECTOR-2-ABD07-WS      PIC 9(12).
           03  PERIODICIDAD-PAGO-ABD07-WS
                                      PIC 9.
           03  GARANTE-ABD07-WS       PIC 9(2).
           03  FORMA-PAGO-TOTAL-ABD07-WS
                                      PIC 9.
           03  MOROSIDAD-ABD07-WS     PIC 9(3).
           03  NOV-REPORT2-ABD07-WS   PIC 9(02).
           03  VECTOR-2-R-ABD07-WS    PIC 9(12).                        VHF0003

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
