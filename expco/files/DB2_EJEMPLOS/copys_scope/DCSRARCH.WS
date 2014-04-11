REMARK*DCSRARCH.WS.v.01.0020.
REMARK******************************************************************
REMARK*    VARIABLES PARA EL MANEJO DE ARCHIVOS EXTERNOS EN DECISOR.   *
REMARK******************************************************************
REMARK* EHA0002     *RQ3237  - SE INCLUYE ARCHIVO NO DESEADOS PARA     *
REMARK* SEP-26-2011 *          COLPATRIA VEHICULOS.                    *
REMARK******************************************************************
REMARK* CIG0003     | RQ3132 - SE INCLUYE EL ARCHIVO DE NO DESEADOS    *
REMARK* 09-28-2011  |          BBVA.                                   *
REMARK******************************************************************
REMARK* CAV0004     | RQ3449 - SE INCLUYE EL ARCHIVO DE NO DESEADOS    *
REMARK* 16-NOV-2011 |          BCSC.                                   *
REMARK******************************************************************
REMARK* CIG0005     | RQ3921 - SE INCLUYE EL ARCHIVO PARA EL CITIBANK  *
REMARK* 29-NOV-2011 |                                                  *
REMARK******************************************************************
REMARK* EHA0006     | RQ3909 - SE INCLUYE ARCHIVO DE GIROS Y FINANZAS  *
REMARK* 03-DIC-2011 |                                                  *
REMARK******************************************************************
REMARK* EHA0007     | RQ4070 - SE INCLUYE ARCHIVO DE BANCO DE BOGOTA   *
REMARK* 22-DIC-2011 |          REQ.4070                                *
REMARK******************************************************************
REMARK* CAV0008     | RQ5926 - SE INCLUYE ARCHIVO DE BANCO COLPATRIA   *
REMARK* 16-MAY-2012 |          RETENCION CUPOS                         *
REMARK******************************************************************
REMARK* IMS0009     | CQ5863 - SE INCLUYE LAS CARACTERSITICAS DEFINIDAS*
REMARK* 15-AGO-2012 |          EN LA SOLICITUD 5863. ETB.              *
REMARK******************************************************************
REMARK* IMS0010     | CQ7065 - SE INCLUYE ARCHIVO DE SUSCRIPTORES PARA *
REMARK* 05-SEP-2012 |          JURISCOOP.                              *
REMARK******************************************************************
REMARK* IMS0011     | CQ7064 - SE INCLUYE LAS CARACTERSITICAS DEFINIDAS*
REMARK* 17-AGO-2012 |          EN LA SOLICITUD 7064. DAVIVIENDA        *
REMARK******************************************************************
REMARK* IMS0012     | CQ7215 - SE INCLUYE ARCHIVO DE PARAMETROS PARA   *
REMARK* 18-SEP-2012 |          PERSONALIZACION DE COLPATRIA.           *
REMARK******************************************************************
REMARK* IMS0013     | CQ7833 - SE AGREGA LA EXTENCION .DAT AL ARCHIVO  *
REMARK* 25-SEP-2012 |          INDEXADO DE NO DESEADOS DE BCSC.        *
REMARK******************************************************************
REMARK* CAV0014     | CQ8189 - SE INCLUYE ARCHIVO DE UVR Y TASAS PARA  *
REMARK* 26-OCT-2012 |          FONDO NACIONAL DEL AHORRO.              *
REMARK******************************************************************
REMARK* CAV0015     | CQ8603 - SE INCLUYE ARCHIVO DE CLIENTES FERIA    *
REMARK* 31-OCT-2012 |          PARA CITI PER 003.                      *
REMARK******************************************************************
REMARK* IMS0016     | CQ8834 - SE AJUSTA EL NOMBRE DEL ARCHIVO PARA    *
REMARK* 13-NOV-2012 |          LAS CARATCERISTICAS DE CITI.            *
REMARK******************************************************************
REMARK* IMS0017     | CQ8984 - SE AJUSTA EL NOMBRE DEL ARCHIVO DE UVR  *
REMARK* 20-NOV-2012 |          Y TASAS DE FNA.                         *
REMARK******************************************************************
REMARK* IMS0018     | CQ9050 - SE INCLUYE ARCHIVO DE PARAMETROS PARA   *
REMARK* 17-DIC-2012 |          CARACTERISTICAS DE DAVIVIENDA.          *
REMARK******************************************************************
REMARK* CAV0019     | CQ9566 - SE INCLUYEN ARCHIVOS DE EMPLEADOS  Y    *
REMARK* 22-ENE-2013 |          CUOTAS X MILLON PARA DAVIVIENDA.        *
REMARK******************************************************************
REMARK* STT0020     | R11058 - SE INCLUYE ARCHIVO CREDITITULOS         *
REMARK* 22-ABR-2013 |                                                  *
REMARK******************************************************************
       01  W-VARIABLES-DCSRARCH.
         03  ARCHIVO-CLIENTCITI-DCSRARCH.
      * ARCHIVO DE CALIFICACIONES CITIBANK.
      * $DATOS/citidat.prn.DAT
           05                          PIC  X(07) VALUE "$DATOS/".
           05                          PIC  X(15)
                                          VALUE "citidat.prn.DAT".
         03  SW-CLIENTCITI-DCSRARCH    PIC  9(01)  VALUE  ZEROS.
         03  W-STATUS-ENT-DCSRARCH     PIC  9(02)  VALUE  ZEROS.
         03                      REDEFINES  W-STATUS-ENT-DCSRARCH.
         05  W-STATUS-ENT-9-DCSRARCH   PIC  9(01).
         05  W-STATUS-ENT-X-DCSRARCH   PIC  9(02)  COMP.
RQ3237*                                                                 EHA0002
RQ3237   03  ARCHIVO-NODESEADOS-COLP.                                   EHA0002
RQ3237     05                          PIC  X(07)  VALUE "$DATOS/".     EHA0002
RQ3237     05                          PIC  X(30)                       EHA0002
RQ3237                              VALUE "Clientes_no_vinculables.prn".EHA0002
RQ3237*                                                                 EHA0002
RQ3237   03  W-STATUS-ENT-COLP         PIC  9(02)    VALUE  ZEROS.      EHA0002
RQ3237   03                      REDEFINES  W-STATUS-ENT-COLP.          EHA0002
RQ3237     05  W-STATUS-ENT-9-COLP          PIC  9(01).                 EHA0002
RQ3237     05  W-STATUS-ENT-X-COLP          PIC  9(02)    COMP.         EHA0002
RQ3237*                                                                 EHA0002
RQ3132   03  ARCHIVO-NODESEADOS-BBVA.                                   CIG0003
RQ3132     05                          PIC  X(07)  VALUE "$DATOS/".     CIG0003
RQ3132     05                          PIC  X(19)                       CIG0003
RQ3132                                 VALUE "ARCHIVO-SCR-WEB.DAT".     CIG0003
RQ3132   03  W-STATUS-ENT-BBVA         PIC  9(02)    VALUE  ZEROS.      CIG0003
RQ3132   03                      REDEFINES  W-STATUS-ENT-BBVA.          CIG0003
RQ3132     05  W-STATUS-ENT-9-BBVA          PIC  9(01).                 CIG0003
RQ3132     05  W-STATUS-ENT-X-BBVA          PIC  9(02)    COMP.         CIG0003
RQ3132*                                                                 CIG0003
RQ3449   03  ARCHIVO-NODESEADOS-BCSC.                                   CAV0004
RQ3449     05                          PIC  X(07)  VALUE "$DATOS/".     CAV0004
CQ7833     05                          PIC  X(24)                       IMS0013
CQ7833                                VALUE "ARCH-NODESEADOS-BCSC.DAT". IMS0013
CQ7833*RQ3449 05                       PIC  X(20)                       CAV0004
CQ7833*RQ3449                         VALUE "ARCH-NODESEADOS-BCSC".     IMS0013
RQ3449   03  W-STATUS-ENT-BCSC         PIC  9(02)    VALUE  ZEROS.      CAV0004
RQ3449   03                      REDEFINES  W-STATUS-ENT-BCSC.          CAV0004
RQ3449     05  W-STATUS-ENT-9-BCSC          PIC  9(01).                 CAV0004
RQ3449     05  W-STATUS-ENT-X-BCSC          PIC  9(02)    COMP.         CAV0004
RQ3449*                                                                 CAV0004
RQ3921   03 ARCH-FACTOR PIC X(100) VALUE "$DATOS/TABFACTOR-CITI.DAT".   CIG0005
CQ5220   03 ARCH-SUSCR  PIC X(100) VALUE "$DATOS/TABSUSCR-CITI.DAT".
CQ7065   03 ARCH-SUSCR-JUR  PIC X(100)                                  IMS0010
CQ7065             VALUE "$DATOS/TABSUSCR-JURISCOOP.DAT".               IMS0010
RQ3909   03  ARCHIVO-NODESEADOS-GYF.                                    EHA0006
RQ3909     05                          PIC  X(07)  VALUE "$DATOS/".     EHA0006
RQ3909     05                          PIC  X(23)                       EHA0006
RQ3909                                 VALUE "ARCH-NODESEADOS-GYF.DAT". EHA0006
RQ3909   03  W-STATUS-ENT-GYF         PIC  9(02)    VALUE  ZEROS.       EHA0006
RQ3909   03                      REDEFINES  W-STATUS-ENT-GYF.           EHA0006
RQ3909     05  W-STATUS-ENT-9-GYF           PIC  9(01).                 EHA0006
RQ3909     05  W-STATUS-ENT-X-GYF           PIC  9(02)    COMP.         EHA0006
RQ3909*                                                                 EHA0006
RQ4070   03  ARCHIVO-NODESEADOS-BCOBOG.                                 EHA0007
RQ4070     05                          PIC  X(07)  VALUE "$DATOS/".     EHA0007
RQ4070     05                          PIC  X(24)                       EHA0007
RQ4070                              VALUE "ARCHIVO_CALIF_BCOBOG.DAT".   EHA0007
RQ4070*                                                                 EHA0007
RQ4070   03  W-STATUS-ENT-BCOBOG       PIC  9(02)    VALUE  ZEROS.      EHA0007
RQ4070   03                      REDEFINES  W-STATUS-ENT-BCOBOG.        EHA0007
RQ4070     05  W-STATUS-ENT-9-BCOBOG        PIC  9(01).                 EHA0007
RQ4070     05  W-STATUS-ENT-X-BCOBOG        PIC  9(02)    COMP.         EHA0007
RQ4070*                                                                 EHA0007
RQ5926   03  ARCHIVO-RETENCION-CUP-COLP.                                CAV0008
RQ5926     05                          PIC  X(07)  VALUE "$DATOS/".     CAV0008
RQ5926     05                          PIC  X(30)                       CAV0008
RQ5926                          VALUE "COLPATRIA_RETENCIONES_CUPO.DAT". CAV0008
RQ5926*                                                                 CAV0008
RQ5926   03  W-STATUS-ENT-CUP-COLP     PIC  9(02)    VALUE  ZEROS.      CAV0008
RQ5926   03                      REDEFINES  W-STATUS-ENT-CUP-COLP.      CAV0008
RQ5926     05  W-STATUS-ENT-9-CUP-COLP      PIC  9(01).                 CAV0008
RQ5926     05  W-STATUS-ENT-X-CUP-COLP      PIC  9(02)    COMP.         CAV0008
RQ5926*                                                                 CAV0008
CQ5863   03  ARCHIVO-VARPROPIAS-ETB.                                    IMS0009
CQ5863     05                          PIC  X(07)  VALUE "$DATOS/".     IMS0009
CQ5863     05                          PIC  X(30)                       IMS0009
CQ5863                          VALUE "ETB_VARIABLES_PROPIAS.DAT ".     IMS0009
CQ5863*                                                                 IMS0009
CQ5863   03  W-STATUS-ENT-VP-ETB       PIC  9(02)    VALUE  ZEROS.      IMS0009
CQ5863   03                      REDEFINES  W-STATUS-ENT-VP-ETB.        IMS0009
CQ5863     05  W-STATUS-ENT-9-VP-ETB        PIC  9(01).                 IMS0009
CQ5863     05  W-STATUS-ENT-X-VP-ETB        PIC  9(02)    COMP.         IMS0009
CQ5863*                                                                 IMS0009
CQ7064   03 ARCHIVO-NODESEADOS-DAV.                                     IMS0011
CQ7064     05           PIC  X(07) VALUE "$DATOS/".                     IMS0011
CQ7064     05           PIC  X(30) VALUE "Dav_No_Deseados.DAT".         IMS0011
CQ7064                                                                  IMS0011
CQ7064   03 W-STATUS-ENT-DAV          PIC  9(02)    VALUE  ZEROS.       IMS0011
CQ7064   03                      REDEFINES  W-STATUS-ENT-DAV.           IMS0011
CQ7064     05  W-STATUS-ENT-9-DAV           PIC  9(01).                 IMS0011
CQ7064     05  W-STATUS-ENT-X-DAV           PIC  9(02)    COMP.         IMS0011
CQ7064*                                                                 IMS0011
CQ7064   03 ARCHIVO-VIP-DAV.                                            IMS0011
CQ7064     05           PIC  X(07) VALUE "$DATOS/".                     IMS0011
CQ7064     05           PIC  X(30) VALUE "Dav_VIP.DAT".                 IMS0011
CQ7064                                                                  IMS0011
CQ7064   03 W-STATUS-VIP-DAV          PIC  9(02)    VALUE  ZEROS.       IMS0011
CQ7064   03                      REDEFINES  W-STATUS-VIP-DAV.           IMS0011
CQ7064     05  W-STATUS-VIP-9-DAV           PIC  9(01).                 IMS0011
CQ7064     05  W-STATUS-VIP-X-DAV           PIC  9(02)    COMP.         IMS0011
CQ7064*                                                                 IMS0011
CQ7215   03  ARCH-PAR-ICGD61.                                           IMS0012
CQ7215     05                         PIC  X(07) VALUE "$DATOS/".       IMS0012
CQ7215     05                         PIC  X(30)                        IMS0012
CQ7215                                   VALUE "PARAM-ICGD61.DAT".      IMS0012
CQ7215                                                                  IMS0012
CQ7215   03  W-STATUS-ICGD61          PIC  9(02)    VALUE  ZEROS.       IMS0012
CQ7215   03                      REDEFINES  W-STATUS-ICGD61.            IMS0012
CQ7215     05  W-STATUS-ENT-9-D61     PIC  9(01).                       IMS0012
CQ7215     05  W-STATUS-ENT-X-D61     PIC  9(02)    COMP.               IMS0012
CQ7215*                                                                 IMS0012
CQ8189   03 SW-EXIT-FNAUVRTASA        PIC  9(01) VALUE  ZEROS.          CAV0014
CQ8189   03 ARCH-UVR-FNA.                                               CAV0014
CQ8189     05           PIC  X(07) VALUE "$DATOS/".                     CAV0014
CQ8984     05           PIC  X(30) VALUE "FNAUVRTASA.DAT".              IMS0017
CQ8984*CQ8189 05        PIC  X(30) VALUE "FNAUVRTASA".                  IMS0017
CQ8189                                                                  CAV0014
CQ8189   03 W-STATUS-FNAUVRTASA       PIC  9(02)    VALUE  ZEROS.       CAV0014
CQ8189   03                      REDEFINES  W-STATUS-FNAUVRTASA.        CAV0014
CQ8189     05  W-STATUS-9-FNA         PIC  9(01).                       CAV0014
CQ8189     05  W-STATUS-X-FNA         PIC  9(02)    COMP.               CAV0014
CQ8189*                                                                 CAV0014
CQ8603   03 SW-EXIST-CITICLIFERIA     PIC  9(01) VALUE  ZEROS.          CAV0015
CQ8603   03 ARCH-CITICLIFERIA.                                          CAV0015
CQ8603     05           PIC  X(07) VALUE "$DATOS/".                     CAV0015
CQ8834     05           PIC  X(30) VALUE "CITICLIFERIA.DAT".            IMS0016
CQ8834*CQ8603 05        PIC  X(30) VALUE "CITICLIFERIA".                IMS0016
CQ8603                                                                  CAV0015
CQ8603   03 W-STATUS-CITICLIFERIA     PIC  9(02)    VALUE  ZEROS.       CAV0015
CQ8603   03                      REDEFINES  W-STATUS-CITICLIFERIA.      CAV0015
CQ8603     05  W-STATUS-9-CITIFER     PIC  9(01).                       CAV0015
CQ8603     05  W-STATUS-X-CITIFER     PIC  9(02)    COMP.               CAV0015
R11058*                                                                 STT0020
R11058   03 SW-EXIST-CREDITITULOS     PIC  9(01) VALUE  ZEROS.          STT0020
R11058   03 ARCH-CREDITITULOS.                                          STT0020
R11058     05           PIC  X(07) VALUE "$DATOS/".                     STT0020
R11058     05           PIC  X(30) VALUE "CREDITITULOS.DAT".            STT0020
R11058                                                                  STT0020
R11058   03 W-STATUS-CREDITITULOS     PIC  9(02)    VALUE  ZEROS.       STT0020
R11058   03                      REDEFINES  W-STATUS-CREDITITULOS.      STT0020
R11058     05  W-STATUS-9-CREDITITUL  PIC  9(01).                       STT0020
R11058     05  W-STATUS-X-CREDITITUL  PIC  9(02)    COMP.               STT0020
CQ8603*                                                                 CAV0015
CQ9050   03 ARCH-PAR-CARACT-DAV.                                        IMS0018
CQ9050     05           PIC  X(07) VALUE "$DATOS/".                     IMS0018
CQ9050     05           PIC  X(30) VALUE "Dav_Param_Caract.DAT".        IMS0018
CQ9050                                                                  IMS0018
CQ9050   03 W-STATUS-PAR-DAV          PIC  9(02)    VALUE  ZEROS.       IMS0018
CQ9050   03                      REDEFINES  W-STATUS-PAR-DAV.           IMS0018
CQ9050     05  W-STATUS-PAR-9-DAV           PIC  9(01).                 IMS0018
CQ9050     05  W-STATUS-PAR-X-DAV           PIC  9(02)    COMP.         IMS0018
CQ9050*                                                                 IMS0018
CQ9566   03 ARCHIVO-EMPL-DAV.                                           CAV0019
CQ9566     05           PIC  X(07) VALUE "$DATOS/".                     CAV0019
CQ9566     05           PIC  X(30) VALUE "Dav_empleados.DAT".           CAV0019
CQ9566                                                                  CAV0019
CQ9566   03 W-STATUS-EMPL-DAV         PIC  9(02)    VALUE  ZEROS.       CAV0019
CQ9566   03                      REDEFINES  W-STATUS-EMPL-DAV.          CAV0019
CQ9566     05  W-STATUS-EMPL-9-DAV           PIC  9(01).                CAV0019
CQ9566     05  W-STATUS-EMPL-X-DAV           PIC  9(02)    COMP.        CAV0019
CQ9566*                                                                 CAV0019
CQ9566   03 ARCHIVO-CUXM-DAV.                                           CAV0019
CQ9566     05           PIC  X(07) VALUE "$DATOS/".                     CAV0019
CQ9566     05           PIC  X(30) VALUE "Dav_cuota_x_mill.DAT".        CAV0019
CQ9566                                                                  CAV0019
CQ9566   03 W-STATUS-CUXM-DAV         PIC  9(02)    VALUE  ZEROS.       CAV0019
CQ9566   03                      REDEFINES  W-STATUS-CUXM-DAV.          CAV0019
CQ9566     05  W-STATUS-CUXM-9-DAV           PIC  9(01).                CAV0019
CQ9566     05  W-STATUS-CUXM-X-DAV           PIC  9(02)    COMP.        CAV0019
CQ9566*                                                                 CAV0019
