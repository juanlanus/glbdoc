      *DCSRARCH.FS.v.01.0017.
REMARK******************************************************************
REMARK*    FD PARA EL MANEJO DE ARCHIVOS EXTERNOS EN DECISOR.          *
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
REMARK* CAV0013     | CQ8189 - SE INCLUYE ARCHIVO DE UVR Y TASAS PARA  *
REMARK* 26-OCT-2012 |          FONDO NACIONAL DEL AHORRO.              *
REMARK******************************************************************
REMARK* CAV0014     | CQ8603 - SE INCLUYE ARCHIVO DE CLIENTES FERIA    *
REMARK* 31-OCT-2012 |          PARA CITI PER 003.                      *
REMARK******************************************************************
REMARK* IMS0015     | CQ9050 - SE INCLUYE ARCHIVO DE PARAMETROS PARA   *
REMARK* 17-DIC-2012 |          CARACTERISTICAS DE DAVIVIENDA.          *
REMARK******************************************************************
REMARK* CAV0016     | CQ9566 - SE INCLUYEN ARCHIVOS DE EMPLEADOS  Y    *
REMARK* 22-ENE-2013 |          CUOTAS X MILLON PARA DAVIVIENDA.        *
REMARK******************************************************************
REMARK* STT0017     | R11058 - SE INCLUYE ARCHIVO CREDITITULOS         *
REMARK* 22-ABR-2013 |                                                  *
REMARK******************************************************************
       $SET IDXFORMAT"0"
      * ARCHIVO DE CALIFICACIONES CITIBANK.
      * $DATOS/citidat.prn.DAT
           SELECT  CLIENTCITI  ASSIGN  TO  ARCHIVO-CLIENTCITI-DCSRARCH
                   ORGANIZATION        IS  INDEXED
                   ACCESS MODE         IS  DYNAMIC
                   RECORD KEY          IS  CLTCIT-NUME-ID
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   FILE STATUS         IS  W-STATUS-ENT-DCSRARCH.
       $SET IDXFORMAT"0"
RQ3237*                                                                 EHA0002
RQ3237 SELECT NODESEADOS-COLP    ASSIGN TO  ARCHIVO-NODESEADOS-COLP     EHA0002
RQ3237        ORGANIZATION         IS  INDEXED                          EHA0002
RQ3237        ACCESS MODE          IS  DYNAMIC                          EHA0002
RQ3237        RECORD KEY           IS  NODESE-NUME-ID-COLP              EHA0002
RQ3237        FILE STATUS          IS  W-STATUS-ENT-COLP.               EHA0002
RQ3237*                                                                 EHA0002
RQ3132 SELECT NODESEADOS-BBVA    ASSIGN TO  ARCHIVO-NODESEADOS-BBVA     CIG0003
RQ3132        ORGANIZATION         IS  INDEXED                          CIG0003
RQ3132        ACCESS MODE          IS  DYNAMIC                          CIG0003
RQ3132        RECORD KEY           IS  NODESE-NUME-ID-BBVA              CIG0003
RQ3132        FILE STATUS          IS  W-STATUS-ENT-BBVA.               CIG0003
RQ3132*                                                                 CIG0003
RQ3449 SELECT NODESEADOS-BCSC    ASSIGN TO  ARCHIVO-NODESEADOS-BCSC     CAV0004
RQ3449        ORGANIZATION         IS  INDEXED                          CAV0004
RQ3449        ACCESS MODE          IS  DYNAMIC                          CAV0004
RQ3449        RECORD KEY           IS  NODESE-NUME-ID-BCSC              CAV0004
RQ3449        FILE STATUS          IS  W-STATUS-ENT-BCSC.               CAV0004
RQ3449*                                                                 CAV0004
RQ3909 SELECT NODESEADOS-GYF     ASSIGN TO  ARCHIVO-NODESEADOS-GYF      EHA0006
RQ3909        ORGANIZATION         IS  INDEXED                          EHA0006
RQ3909        ACCESS MODE          IS  DYNAMIC                          EHA0006
RQ3909        RECORD KEY           IS  NODESE-NUME-ID-GYF               EHA0006
RQ3909        FILE STATUS          IS  W-STATUS-ENT-GYF.                EHA0006
RQ3909*                                                                 EHA0006
RQ3921 SELECT TABFACTOR-C        ASSIGN TO DISK ARCH-FACTOR             CIG0005
RQ3921        ORGANIZATION         IS LINE SEQUENTIAL.                  CIG0005

CQ5220 SELECT TABSUSCR-C         ASSIGN TO DISK ARCH-SUSCR
CQ5220        ORGANIZATION         IS LINE SEQUENTIAL.
CQ7065*                                                                 IMS0010
CQ7065 SELECT TABSUSCR-JUR       ASSIGN TO DISK ARCH-SUSCR-JUR          IMS0010
CQ7065        ORGANIZATION         IS LINE SEQUENTIAL.                  IMS0010
RQ4070*                                                                 EHA0007
RQ4070 SELECT NODESEADOS-BCOBOG  ASSIGN TO  ARCHIVO-NODESEADOS-BCOBOG   EHA0007
RQ4070        ORGANIZATION         IS  INDEXED                          EHA0007
RQ4070        ACCESS MODE          IS  DYNAMIC                          EHA0007
RQ4070        RECORD KEY           IS  NODESE-NUME-ID-BCOBOG            EHA0007
RQ4070        FILE STATUS          IS  W-STATUS-ENT-BCOBOG.             EHA0007
RQ4070*                                                                 EHA0007
RQ5926 SELECT RETENCION-CUP-COLP  ASSIGN TO ARCHIVO-RETENCION-CUP-COLP  CAV0008
RQ5926        ORGANIZATION         IS  INDEXED                          CAV0008
RQ5926        ACCESS MODE          IS  DYNAMIC                          CAV0008
RQ5926        RECORD KEY           IS  RETENCION-CUP-ID-COLP            CAV0008
RQ5926        FILE STATUS          IS  W-STATUS-ENT-CUP-COLP.           CAV0008
RQ5926*                                                                 CAV0008
CQ5863 SELECT VARPROPIAS-ETB      ASSIGN TO ARCHIVO-VARPROPIAS-ETB      IMS0009
CQ5863        ORGANIZATION         IS  INDEXED                          IMS0009
CQ5863        ACCESS MODE          IS  DYNAMIC                          IMS0009
CQ5863        RECORD KEY           IS  VARPROPIAS-NUME-ID-ETB           IMS0009
CQ5863        FILE STATUS          IS  W-STATUS-ENT-VP-ETB.             IMS0009
CQ5863*                                                                 IMS0009
CQ7064 SELECT NODESEADOS-DAV    ASSIGN TO  ARCHIVO-NODESEADOS-DAV       IMS0011
CQ7064        ORGANIZATION         IS  INDEXED                          IMS0011
CQ7064        ACCESS MODE          IS  DYNAMIC                          IMS0011
CQ7064        RECORD KEY           IS  NODESE-NUME-ID-DAV               IMS0011
CQ7064        FILE STATUS          IS  W-STATUS-ENT-DAV.                IMS0011
CQ7064*                                                                 IMS0011
CQ7064 SELECT VIP-DAV           ASSIGN TO  ARCHIVO-VIP-DAV              IMS0011
CQ7064        ORGANIZATION         IS  INDEXED                          IMS0011
CQ7064        ACCESS MODE          IS  DYNAMIC                          IMS0011
CQ7064        RECORD KEY           IS  VIP-NUME-ID-DAV                  IMS0011
CQ7064        FILE STATUS          IS  W-STATUS-VIP-DAV.                IMS0011
CQ7064*                                                                 IMS0011
CQ7215 SELECT PAR-ICGD61     ASSIGN TO  ARCH-PAR-ICGD61                 IMS0012
CQ7215           ORGANIZATION          IS  LINE SEQUENTIAL              IMS0012
CQ7215           ACCESS MODE           IS  SEQUENTIAL                   IMS0012
CQ7215           FILE STATUS           IS  W-STATUS-ICGD61.             IMS0012
CQ7215*                                                                 IMS0012
CQ8189 SELECT FNAUVRTASA     ASSIGN TO  ARCH-UVR-FNA                    CAV0013
CQ8189        ORGANIZATION         IS  INDEXED                          CAV0013
CQ8189        ACCESS MODE          IS  DYNAMIC                          CAV0013
CQ8189        RECORD KEY           IS  LLAVE-FNAUVRTASA                 CAV0013
CQ8189        FILE STATUS          IS  W-STATUS-FNAUVRTASA.             CAV0013
CQ8189*                                                                 CAV0013
CQ8603 SELECT CITICLIFERIA   ASSIGN TO  ARCH-CITICLIFERIA               CAV0014
CQ8603        ORGANIZATION         IS  INDEXED                          CAV0014
CQ8603        ACCESS MODE          IS  DYNAMIC                          CAV0014
CQ8603        RECORD KEY           IS  LLAVE-CITICLIFERIA               CAV0014
CQ8603        FILE STATUS          IS  W-STATUS-CITICLIFERIA.           CAV0014
R11058*                                                                 STT0017
R11058 SELECT CREDITITULOS   ASSIGN TO  ARCH-CREDITITULOS               STT0017
R11058        ORGANIZATION         IS  INDEXED                          STT0017
R11058        ACCESS MODE          IS  DYNAMIC                          STT0017
R11058        RECORD KEY           IS  LLAVE-CREDITITULOS               STT0017
R11058        FILE STATUS          IS  W-STATUS-CREDITITULOS.           STT0017
CQ8603*                                                                 CAV0014
CQ9050 SELECT PAR-CARACT-DAV    ASSIGN TO  ARCH-PAR-CARACT-DAV          IMS0015
CQ9050           ORGANIZATION          IS  LINE SEQUENTIAL              IMS0015
CQ9050           ACCESS MODE           IS  SEQUENTIAL                   IMS0015
CQ9050           FILE STATUS           IS  W-STATUS-PAR-DAV.            IMS0015
CQ9050*                                                                 IMS0015
CQ9566 SELECT EMPLEADOS-DAV    ASSIGN TO  ARCHIVO-EMPL-DAV              CAV0016
CQ9566        ORGANIZATION         IS  INDEXED                          CAV0016
CQ9566        ACCESS MODE          IS  DYNAMIC                          CAV0016
CQ9566        RECORD KEY           IS  EMPLE-NUME-ID-DAV                CAV0016
CQ9566        FILE STATUS          IS  W-STATUS-EMPL-DAV.               CAV0016
CQ9566*                                                                 CAV0016
CQ9566 SELECT CUO-X-MLL-DAV    ASSIGN TO  ARCHIVO-CUXM-DAV              CAV0016
CQ9566        ORGANIZATION         IS  LINE SEQUENTIAL                  CAV0016
CQ9566        ACCESS MODE          IS  SEQUENTIAL                       CAV0016
CQ9566        FILE STATUS          IS  W-STATUS-CUXM-DAV.               CAV0016
CQ9566*                                                                 CAV0016
