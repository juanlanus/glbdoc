REMARK*CARACTPER-WS.0022.
REMARK**********************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS                     
REMARK**********************************************************************
REMARK* EHA0002     * RQ3218 - SE INCLUYEN VARIABLES PARA COLPATRIA    *
REMARK* SEPT.22/2011*          TARJETAS BLACK REQ.3218                 * 
REMARK******************************************************************
REMARK* EHA0003     * RQ3237 - SE INCLUYEN VARIABLES PARA COLPATRIA    *
REMARK* SEPT.24/2011*          VEHICULOS REQ.3237                      * 
REMARK******************************************************************
REMARK* CIG0004     | RQ3132 - SE INCLUYE EL ARCHIVO DE NO DESEADOS    *
REMARK* 09-29-2011  |          BBVA.                                   *
REMARK******************************************************************
REMARK* EHA0005     | RQ3605 - SE INCLUYEN VARIABLES DEL BANCO         *
REMARK* 08-NOV-2011 |          COLPATRIA.                              *
REMARK******************************************************************
REMARK* CAV0006     | RQ3449 - SE INCLUYE COPY CON VARIABLES DEL BANCO *
REMARK* 15-NOV-2011 |          CAJA SOCIAL BCSC.                       *
REMARK******************************************************************
REMARK* EHA0007     | RQ3702 - SE INCLUYE COPY CON VARIABLES DE        *
REMARK* 15-NOV-2011 |          FINANCIERA INTERNACIONAL.               *
REMARK******************************************************************
REMARK* STT0008     | RQCITI - SE INCLUYE COPY CON VARIABLES DE        *
REMARK* 24-NOV-2011 |          CITIBANCK                               *
REMARK******************************************************************
REMARK* CIG0009     | RQ3921 -SE INCLUYE COPYS DE LAS PERSONALIZACIONES*
REMARK* 28-NOV-2011 | 01,02,04,05.                                     *
REMARK******************************************************************
      * NDG0010    | JAVA02 - ARREGLO QUE PERMITE MANEJAR LOS DATOS QUE*
      * FEB.15.2012|          DEVUELBE EL LLAMADO AL COMPONENTE JAVA Y *
      * N.DUARTE   |          PERMITE INDEPENDIZAR EL LLAMADO EN SOLO  *
      *            |          JAVA PARA DECISOR-3.                     *
      ******************************************************************
      * IMS0011     | CQ5863 - SE INCLUYE COPY PARA EL CALCULO DE      *
      * 15-AGO-2012 |          CARACTERISTICAS DE ETB PERSON. 21       *
      ******************************************************************
      * IMS0012     | CQ6485 - SE INCLUYE COPY PARA EL CALCULO DE      *
      * 12-JUL-2012 |          CARACTERISTICAS DE FINESA. PERSO 22     *
      ******************************************************************
      * CAV0013     | CQ6555 - SE INCLUYE COPY PARA EL CALCULO DE      *
      * 27-JUL-2012 |          CARACTERISTICAS DE SUFINANCIAMIENTO 23  *
      ******************************************************************
      * IMS0014     | CQ7064 - SE INCLUYE COPY PARA EL CALCULO DE      *
      * 15-AGO-2012 |          CARACTERISTICAS DE DAVIVIENDA PERSON. 25*
      ******************************************************************
      * CAV0015     | CQ7816 - SE INCLUYE COPY PARA EL CALCULO DE      *
      * 11-OCT-2012 |          CARACTERISTICAS DE GMAC PERSONALIZ 27   *
      * CAV0015     | CQ7817 - SE INCLUYE COPY PARA EL CALCULO DE      *
      * 11-OCT-2012 |          CARACTERISTICAS DE FERRETERIA AL DIA P26*
      ******************************************************************
      * CAV0016     | CQ8189 - SE INCLUYE ARCHIVO DE UVR Y TASAS PARA  *
      * 26-OCT-2012 |          FONDO NACIONAL DEL AHORRO.              *
      ******************************************************************
      * NDG0017     | CQ8603 - SE AMPLIA EL VALOR DE LA VARIABLE DE LAS*
      * 01-NOV-2012 |          PERSONALIZACIONES V-VARIABLES-PER.      *
      ******************************************************************
      * IMS0018     | CQ9491 - SE INCLUYE COPY PARA EL CALCULO DE      *
      * 21-DIC-2012 |          CARACTERISTICAS DE RIPLAY PERSON. 29    *
      ******************************************************************
      * IMS0019     | CQ9773 - SE AGREGAN COPYS EN BLANCO PARA LAS     *
      * 22-ENE-2013 |          PERSONALIZACIONES 30 A 40.              *
      ******************************************************************
      * STT0020     | R12051 - SE AGREGAN COPYS PARA PERSONALIZACIONES *
      * 06-MAY-2013 |          42 Y 43                                 *
      ******************************************************************
      * STT0021     | R11058 - SE AGREGAN COPYS PARA CREDITITULOS      *
      * 22-ABR-2013 |          PERSONALIZACION 41.                     *
      ******************************************************************
      * IMS0022     | R12212 - SE AGREGAN COPYS PARA NATURA            *
      * 06-JUN-2013 |          PERSONALIZACION 45.                     *
      * M.SALAMANCA |                                   REQ. CQ. 12212.*
      ******************************************************************

        01 VECTOR-VARIABLES-PER.
RQCITI*    02 V-VARIABLES-PER OCCURS 100 TIMES.                         STT0008
RQCITI*    02 V-VARIABLES-PER OCCURS 120 TIMES.                         STT0008
RQ8603*    02 V-VARIABLES-PER OCCURS 150 TIMES.                         STT0008
RQ8603     02 V-VARIABLES-PER OCCURS 300 TIMES.                         
              03 NOMBRE-VAR-PER         PIC X(15).
              03 VAL-VAR-PER            PIC X(15).
           02 I-VAR-PER                 PIC 9(03).
           02 L-AUX                     PIC 9(02).
        01 NUM-PERSO-CARACT             PIC 9(02).
RQ3921* 01 PERSONALIZACION-01.                                          CIG0009
RQ3921*    02 PER-01-NIVEL-00.                                          CIG0009
RQ3921*       03 00NUM049CT001          PIC 9(3).                       CIG0009
RQ3921*       03 00ACP999XX001          PIC 9(3).                       CIG0009
RQ3921*       03 00MORXXXOT001.                                         CIG0009
RQ3921*          04 00MOR999OT001          PIC 9(3).                    CIG0009
RQ3921*          04 00MOR998OT001          PIC 9(3).                    CIG0009
RQ3921*          04 00MOR997OT001          PIC 9(3).                    CIG0009
RQ3921*          04 00MOR996OT001          PIC 9(3).                    CIG0009
RQ3921*    02 PER-01-NIVEL-01.                                          CIG0009
RQ3921*       03 01NUM049CT001          PIC 9(4).                       CIG0009
RQ3921*       03 01ACP999XX001-M.                                       CIG0009
RQ3921*          04 01ACP999RO001       PIC 9(4).                       CIG0009
RQ3921*          04 01ACP999IN001       PIC 9(4).                       CIG0009
RQ3921*          04 01ACP999VE001       PIC 9(4).                       CIG0009
RQ3921*          04 01ACP999HP001       PIC 9(4).                       CIG0009
RQ3921*          04 01ACP999OT001       PIC 9(4).                       CIG0009
RQ3921*          04 01ACP999CC001       PIC 9(4).                       CIG0009
RQ3921*          04 01ACP999CO001       PIC 9(4).                       CIG0009
RQ3921*          04 01ACP999AH001       PIC 9(4).                       CIG0009
RQ3921*          04 01ACP999CT001       PIC 9(4).                       CIG0009
RQ3921*       03 FILLER REDEFINES 01ACP999XX001-M.                      CIG0009
RQ3921*          04 01ACP999XX001 OCCURS 9 TIMES PIC 9(04).             CIG0009
RQ3921*       03 01MORXXXOT001.                                         CIG0009
RQ3921*          04 01MOR999OT001          PIC 9(4).                    CIG0009
RQ3921*          04 01MOR998OT001          PIC 9(4).                    CIG0009
RQ3921*          04 01MOR997OT001          PIC 9(4).                    CIG0009
RQ3921*          04 01MOR996OT001          PIC 9(4).                    CIG0009
RQ3921*    02 PER-01-NIVEL-02.                                          CIG0009
RQ3921*       03 02ACP999TO001          PIC 9(4).                       CIG0009
RQ3921*       03 02MORXXXTO001.                                         CIG0009
RQ3921*          04 02MOR999TO001          PIC 9(4).                    CIG0009
RQ3921*          04 02MOR998TO001          PIC 9(4).                    CIG0009
RQ3921*          04 02MOR997TO001          PIC 9(4).                    CIG0009
RQ3921*          04 02MOR996TO001          PIC 9(4).                    CIG0009
RQ3921* 01 PERSONALIZACION-02.                                          CIG0009
RQ3921*    02 PER-02-NIVEL-00.                                          CIG0009
RQ3921*       03 00MOR129               PIC 9(3).                       CIG0009
RQ3921*       03 00MOR130               PIC 9(3).                       CIG0009
RQ3921*       03 00MOR131               PIC 9(3).                       CIG0009
RQ3921*       03 00MOR132               PIC 9(3).                       CIG0009
RQ3921*       03 00MOR133               PIC 9(3).                       CIG0009
RQ3921*       03 00MOR134               PIC 9(3).                       CIG0009
RQ3921*       03 00MOR135               PIC 9(3).                       CIG0009
RQ3921*       03 00MOR136               PIC 9(3).                       CIG0009
RQ3921*    02 PER-02-NIVEL-01.                                          CIG0009
RQ3921*       03 VAR-MOR-PER-02.                                        CIG0009
RQ3921*         04 V-01MOR-PER-02       OCCURS 9 TIMES.                 CIG0009
RQ3921*           05 01MOR129       PIC 9(4). *> MORAS  30 ULT 03 MESE  CIG0009
RQ3921*           05 01MOR130       PIC 9(4). *> MORAS  60 ULT 03 MESE  CIG0009
RQ3921*           05 01MOR131       PIC 9(4). *> MORAS  90 ULT 03 MESE  CIG0009
RQ3921*           05 01MOR132       PIC 9(4). *> MORAS 120 ULT 03 MESE  CIG0009
RQ3921*           05 01MOR133       PIC 9(4). *> MORAS  30 ULT 09 MESE  CIG0009
RQ3921*           05 01MOR134       PIC 9(4). *> MORAS  60 ULT 09 MESE  CIG0009
RQ3921*           05 01MOR135       PIC 9(4). *> MORAS  90 ULT 09 MESE  CIG0009
RQ3921*           05 01MOR136       PIC 9(4). *> MORAS 120 ULT 09 MESE  CIG0009
RQ3921*      03 FILLER REDEFINES VAR-MOR-PER-02.                        CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 01MOR129RO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR130RO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR131RO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR132RO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR133RO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR134RO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR135RO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR136RO       PIC 9(4).                              CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 01MOR129IN       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR130IN       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR131IN       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR132IN       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR133IN       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR134IN       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR135IN       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR136IN       PIC 9(4).                              CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 01MOR129VE       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR130VE       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR131VE       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR132VE       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR133VE       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR134VE       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR135VE       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR136VE       PIC 9(4).                              CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 01MOR129HP       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR130HP       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR131HP       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR132HP       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR133HP       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR134HP       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR135HP       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR136HP       PIC 9(4).                              CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 01MOR129OT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR130OT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR131OT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR132OT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR133OT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR134OT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR135OT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR136OT       PIC 9(4).                              CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 01MOR129CC       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR130CC       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR131CC       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR132CC       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR133CC       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR134CC       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR135CC       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR136CC       PIC 9(4).                              CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 01MOR129CO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR130CO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR131CO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR132CO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR133CO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR134CO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR135CO       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR136CO       PIC 9(4).                              CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 01MOR129AH       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR130AH       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR131AH       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR132AH       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR133AH       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR134AH       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR135AH       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR136AH       PIC 9(4).                              CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 01MOR129CT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR130CT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR131CT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR132CT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR133CT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR134CT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR135CT       PIC 9(4).                              CIG0009
RQ3921*      05 01MOR136CT       PIC 9(4).                              CIG0009
RQ3921*    02 PER-02-NIVEL-02.                                          CIG0009
RQ3921*      05 02MOR129TO       PIC 9(4).                              CIG0009
RQ3921*      05 02MOR130TO       PIC 9(4).                              CIG0009
RQ3921*      05 02MOR131TO       PIC 9(4).                              CIG0009
RQ3921*      05 02MOR132TO       PIC 9(4).                              CIG0009
RQ3921*      05 02MOR133TO       PIC 9(4).                              CIG0009
RQ3921*      05 02MOR134TO       PIC 9(4).                              CIG0009
RQ3921*      05 02MOR135TO       PIC 9(4).                              CIG0009
RQ3921*      05 02MOR136TO       PIC 9(4).                              CIG0009
RQ3921* 01 PERSONALIZACION-04.                                          CIG0009
RQ3921*    02 PER-04-NIVEL-01.                                          CIG0009
RQ3921*      05 01NUM017         PIC 9(4).                              CIG0009
RQ3921*      05 99END002TO00     PIC X(01).                             CIG0009
RQ3921*      05 01END999TC004               PIC  9(12) VALUE  ZEROS.    CIG0009
RQ3921*      05 01END998TC004               PIC  9(12) VALUE  ZEROS.    CIG0009
RQ3921*      05 99DEM007TO12                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99DEM001EX61                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99DEM002TO00                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99DEM003TO12                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99DEM002EX61                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99END004TO13                PIC  9(13) VALUE  ZEROS.    CIG0009
RQ3921*      05 99END020EX61                PIC  9(13) VALUE  ZEROS.    CIG0009
RQ3921*      05 04END020TO50                PIC  9(13) VALUE  ZEROS.    CIG0009
RQ3921*      05 99END022EX61                PIC  9(13) VALUE  ZEROS.    CIG0009
RQ3921*      05 99END019TO13                PIC  9(13) VALUE  ZEROS.    CIG0009
RQ3921*      05 99EXP003TC00                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99EXP003TO07                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR001EX61                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR001TO00                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR002EX61                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR002TO00                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR003EX61                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR007TO12                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR008EX61                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR008TO12                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR009EX61                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR009TO12                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR016TO12                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR028EX61                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR028EX04                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99NUM005TO00                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR010TC00                PIC  9(06) VALUE  ZEROS.    CIG0009
RQ3921*      05 99DEM004TO00                PIC  9(03) VALUE  ZEROS.    CIG0009
RQ3921*      05 99DEM001TO00                PIC  9(02) VALUE  ZEROS.    CIG0009
RQ3921*      05 99DEM006TO00                PIC  9(08) VALUE  ZEROS.    CIG0009
RQ3921*      05 99DEM008TO00                PIC  9(01) VALUE  ZEROS.    CIG0009
RQ3921*      05 99MOR004EX61                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 99MOR005EX61                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 99MOR006EX61                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 99MOR007EX61                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 99MOR011TO12                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 99MOR013TO12                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 99MOR014TO12                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 99MOR015TO12                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 99EXP002EX61                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 99EXP045EX04                PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 MORAS30-99EXP002EX61        PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 MORAS60-99EXP002EX61        PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 MORAS90-99EXP002EX61        PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 MORAS120-99EXP002EX61       PIC  9(06)  VALUE ZEROS.    CIG0009
RQ3921*      05 SW-NODESEADOS-COLP          PIC  9(01)  VALUE ZEROS.    CIG0009
RQ3921*                                                                 CIG0009
RQ3921*      05 W-FECHA-SUP                 PIC  9(08) VALUE  ZEROS.    CIG0009
RQ3921*      05 IND-E                       PIC  9(05) VALUE  ZEROS.    CIG0009
RQ3921*01 PERSONALIZACION-05.                                           CIG0009
RQ3921*    02 PER-05-NIVEL-01.                                          CIG0009
RQ3921*      05 SW-NODESEADOS-BBVA          PIC  9(01)  VALUE ZEROS.    CIG0009
RQ3921*      05 SW-SCR-WEB-ANTIG-BBVA       PIC  9(02)  VALUE ZEROS.    CIG0009
RQ3921*      05 SW-SCR-WEB-DOMICILIA-BBVA   PIC  X(01)  VALUE SPACE.    CIG0009
RQ3921*      05 SW-RECIPROCIDAD-SIG-BBVA    PIC  X(02)  VALUE SPACE.    CIG0009
RQ3921*      05 SW-RECIPROCIDAD-ENT-BBVA    PIC  9(13)  VALUE ZEROS.    CIG0009
RQ3921*      05 SW-RECIPROCIDAD-DEC-BBVA    PIC  9(02)  VALUE ZEROS.    CIG0009
RQ3218 01  CARACT-CONSTANTES.                                           EHA0002
RQ3218   03  W-TABLA-TIPO-CTA.                                          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "01CCB2".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "02TDC5".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "03TIC5".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "04CAU1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "05CAB2".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "06CAC3".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "07COF3".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "08CFE3".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "09CVE1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "10CLB1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "11COC1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "12CTU1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "13CAV2".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "14CCL1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "15CCC1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "16CFR1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "17CAF3".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "18CCF3".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "19CMU1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "20CCS1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "21PCS4".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "22CEL1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "23CTC1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "24CDC1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "25DIC1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "26CDB1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "27CAC3".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "28COC1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "29CSP1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "30AGR1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "31ALI1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "32CMZ1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "33CSA4".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "34COM1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "35FER1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "36FUN1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "37GRM1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "38IND1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "39LAB1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "40MSJ1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "41SEG1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "42TRT1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "43EDU1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "44SFI2".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "45CAU1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "46CON2".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "47APD1".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "48SBG2".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "49MCR2".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "500000".          EHA0002
RQ3218     05                     PIC  X(06)   VALUE "51AHO2".          EHA0002
RQ3218   03                   REDEFINES  W-TABLA-TIPO-CTA.              EHA0002
RQ3218     05  W-TBL-TIP-CTA     OCCURS  51  TIMES.                     EHA0002
RQ3218       07  W-TIP-COD        PIC  9(02).                           EHA0002
RQ3218       07  W-TIP-DES        PIC  X(03).                           EHA0002
RQ3218       07  W-CLASE-CTA      PIC  X(01).                           EHA0002
RQ3218   03  CARACT-TABLA-NOVEDADES-CARTERA.                            EHA0002
RQ3218     05          PIC X(23) VALUE  "0101NORMAL.-.AL.DIA1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0202TARJ. NO.RECLAM1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0803CANCEL.MAL.MANE3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0404TARJETA.ROBADA 1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0505CANCEL.VOLUNTAR1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0606CANCEL.MMX<60  1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0707TARJ.EXTRAV.   1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0308PAGO VOLUNT.SAT1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0309PAGO.MMX.A.30  1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0310PAGO.MMX.A.60  1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0311PAGO.MORA.90   1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0312PAGO.VOL.MOR1203   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0113MORA.30.AL.DIA 1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0114MORA.60.AL.DIA 1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0115MORA.90.AL.DIA 1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0116MORA.120.AL.DIA1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1217ESTA.MORA.30   3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1318ESTA.MORA.60   3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1419ESTA.MORA.90   3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1520ESTA.MORA.120  3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1221FU.60.Y.ESTA.303   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1222FU.90.Y.ESTA.303   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1323FU.90.Y.ESTA.603   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1224FU.120.ESTA.30 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1325FU.120.ESTA.60 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1426FU.120.ESTA.90 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1327RE.30.ESTA.60  3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1428RE.30.Y.ESTA.903   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1529RE.30.ESTA.120 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1230RE.60.Y.ESTA.303   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1331RE.60.Y.ESTA.603   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1432RE.60.Y.ESTA.903   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1533RE.60.ESTA.120 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1234RE.90.Y.ESTA.303   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1335RE.90.Y.ESTA.603   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1436RE.90.Y.ESTA.903   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1537RE.90.ESTA.120 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1238RE.120.ESTA.30 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1339RE.120.ESTA.60 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1440RE.120.ESTA.90 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1541RE.120.ESTA.1203   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "2042ADJ.NO.LOCALIZA3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "2143ADJ.CTA.COBRADO3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "2244FALLECIDO      0   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0945CARTERA.CASTIGA3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1046CARTERA.RECUPER3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "1147DUDOSO.RECAUDO 3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "2348CONSULTA       0   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "49                 0   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "50                 0   ".      EHA0002
RQ3218   03              REDEFINES  CARACT-TABLA-NOVEDADES-CARTERA.     EHA0002
RQ3218     05  CARACT-TAB-NOVEDADES-CARTERA OCCURS  50  TIMES.          EHA0002
RQ3218       07  CARACT-TAB-NOV-CART-CODINT PIC 9(02).                  EHA0002
RQ3218       07  CARACT-TAB-NOV-CART-COD    PIC 9(02).                  EHA0002
RQ3218       07  CARACT-TAB-NOV-CART-NOM    PIC X(15).                  EHA0002
RQ3218       07  CARACT-TAB-NOV-CART-CAL    PIC 9(01).                  EHA0002
RQ3218       07  CARACT-TAB-NOV-CART-RAN    PIC 9(02).                  EHA0002
RQ3218       07  CARACT-TAB-NOV-CART-CAL2   PIC 9(01).                  EHA0002
RQ3218*                                                                 EHA0002
RQ3218   03  CARACT-TABLA-NOVEDADES-CTA-CTE.                            EHA0002
RQ3218     05          PIC X(23) VALUE  "3001NORMAL.-.AL.DIA1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "3102CANCEL.MAL.MANE3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0003               0   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0004               0   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "3205SALDADA        1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "3306EMBARGADA      3   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "3507EMBARGADA.NORMA1   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "0008               0   ".      EHA0002
RQ3218     05          PIC X(23) VALUE  "3409INACTIVA       1   ".      EHA0002
RQ3218   03              REDEFINES  CARACT-TABLA-NOVEDADES-CTA-CTE.     EHA0002
RQ3218      05  CARACT-TAB-NOVEDADES-CTA-CTE  OCCURS   9  TIMES.        EHA0002
RQ3218        07  CARACT-TAB-NOV-CTAC-CODINT PIC 9(02).                 EHA0002
RQ3218        07  CARACT-TAB-NOV-CTAC-COD    PIC 9(02).                 EHA0002
RQ3218        07  CARACT-TAB-NOV-CTAC-NOM    PIC X(15).                 EHA0002
RQ3218        07  CARACT-TAB-NOV-CTAC-CAL    PIC 9(01).                 EHA0002
RQ3218        07  CARACT-TAB-NOV-CTAC-RAN    PIC 9(02).                 EHA0002
RQ3218        07  CARACT-TAB-NOV-CTAC-CAL2   PIC 9(01).                 EHA0002
RQ3218*                                                                 EHA0002
RQ3218   03  CARACT-TABLA-VIGENTES-CARTERA.                             EHA0002
RQ3218     05               PIC X(10)   VALUE  "1000000000".            EHA0002
RQ3218     05               PIC X(10)   VALUE  "0011111111".            EHA0002
RQ3218     05               PIC X(10)   VALUE  "1111111111".            EHA0002
RQ3218     05               PIC X(10)   VALUE  "1111111111".            EHA0002
RQ3218     05               PIC X(10)   VALUE  "1111101101".            EHA0002
RQ3218   03              REDEFINES  CARACT-TABLA-VIGENTES-CARTERA.      EHA0002
RQ3218     05  CARACT-TAB-VIGENTES-CARTERA   OCCURS 50 TIMES  PIC 9(01).EHA0002
RQ3218*                                                                 EHA0002
RQ3218  03  CARACT-TABLA-ENDEUDAMIENTO.                                 EHA0002
RQ3218     05             PIC  X(23)  VALUE  "24A NORMAL         1   ". EHA0002
RQ3218     05             PIC  X(23)  VALUE  "25B SUB-NORMAL     3   ". EHA0002
RQ3218     05             PIC  X(23)  VALUE  "26C DEFICIENTE     3   ". EHA0002
RQ3218     05             PIC  X(23)  VALUE  "27D DIFICIL-COBRO  3   ". EHA0002
RQ3218     05             PIC  X(23)  VALUE  "28E IRRECUPERABLE  3   ". EHA0002
RQ3218     05             PIC  X(23)  VALUE  "29+ CONTINGENCIA   0   ". EHA0002
RQ3218     05             PIC  X(23)  VALUE  "00T                0   ". EHA0002
RQ3218     05             PIC  X(23)  VALUE  "00R                0   ". EHA0002
RQ3218   03  CARACT-TABLA-ENDEUDAMIENTO-R  REDEFINES                    EHA0002
RQ3218                                CARACT-TABLA-ENDEUDAMIENTO.       EHA0002
RQ3218     05  CARACT-TAB-ENDEUDAMIENTO      OCCURS   8  TIMES.         EHA0002
RQ3218       07  CARACT-TAB-END-CODINT        PIC  9(02).               EHA0002
RQ3218       07  CARACT-TAB-END-COD           PIC  X(01).               EHA0002
RQ3218       07  CARACT-TAB-END-NOM           PIC  X(16).               EHA0002
RQ3218       07  CARACT-TAB-END-CAL           PIC  9(01).               EHA0002
RQ3218       07  CARACT-TAB-END-RAN           PIC  9(02).               EHA0002
RQ3218       07  CARACT-TAB-END-CAL2          PIC  9(01).               EHA0002
RQ3218*                                                                 EHA0002

JAVA02 01 BDD-CARCTRSTCAS-PER.                                          NDG0010
JAVA02    02 BDD-CARCTRSTCAS-OCC-PER OCCURS 10000.                      NDG0010
JAVA02       03 BDD-CAR-NOM-PER            PIC X(50).                   NDG0010
JAVA02       03 BDD-CAR-VAL-PER            PIC X(50).                   NDG0010

JAVA02 01 I-JAVA02-PER                     PIC 9(06) VALUE ZEROS.       NDG0010
JAVA02 01 J-JAVA02-PER                     PIC 9(06) VALUE ZEROS.       NDG0010
JAVA02 01 K-JAVA02-PER                     PIC 9(06) VALUE ZEROS.       NDG0010
JAVA02 01 L-JAVA02-PER                     PIC 9(06) VALUE ZEROS.       NDG0010
JAVA02 01 M-JAVA02-PER                     PIC 9(06) VALUE ZEROS.       NDG0010

RQ3921  COPY CARACTPER-001.WS.                                          CIG0009
RQ3921  COPY CARACTPER-002.WS.                                          CIG0009
RQCITI  COPY CARACTPER-003.WS.                                          STT0008
RQ3921  COPY CARACTPER-004.WS.                                          CIG0009
RQ3921  COPY CARACTPER-005.WS.                                          CIG0009
RQ3449  COPY CARACTPER-006.WS.                                          CAV0006
RQ3702  COPY CARACTPER-007.WS.                                          EHA0007
RQ3921  COPY CARACTPER-008.WS.                                          CIG0009
RQ3921  COPY CARACTPER-009.WS.                                          CIG0009
RQ3921  COPY CARACTPER-010.WS.                                          CIG0009
RQ3921  COPY CARACTPER-011.WS.                                          CIG0009
RQ3921  COPY CARACTPER-012.WS.                                          CIG0009
RQ3921  COPY CARACTPER-013.WS.                                          CIG0009
RQ3921  COPY CARACTPER-014.WS.                                          CIG0009
RQ3921  COPY CARACTPER-015.WS.                                          CIG0009
RQ3921  COPY CARACTPER-016.WS.                                          CIG0009
RQ3921  COPY CARACTPER-017.WS.                                          CIG0009
RQ3921  COPY CARACTPER-018.WS.                                          CIG0009
RQ3921  COPY CARACTPER-019.WS.                                          CIG0009
RQ3921  COPY CARACTPER-020.WS.                                          CIG0009
CQ5863  COPY CARACTPER-021.WS.                                          IMS0011
CQ6485  COPY CARACTPER-022.WS.                                          IMS0012
CQ6555  COPY CARACTPER-023.WS.                                          CAV0013
CQ7064  COPY CARACTPER-025.WS.                                          IMS0014
CQ7817  COPY CARACTPER-026.WS.                                          CAV0015
CQ7816  COPY CARACTPER-027.WS.                                          CAV0015
CQ8189  COPY CARACTPER-028.WS.                                          CAV0016
CQ9491  COPY CARACTPER-029.WS.                                          IMS0018
CQ9773  COPY CARACTPER-030.WS.                                          IMS0019
CQ9773  COPY CARACTPER-031.WS.                                          IMS0019
CQ9773  COPY CARACTPER-032.WS.                                          IMS0019
CQ9773  COPY CARACTPER-033.WS.                                          IMS0019
CQ9773  COPY CARACTPER-034.WS.                                          IMS0019
CQ9773  COPY CARACTPER-035.WS.                                          IMS0019
CQ9773  COPY CARACTPER-036.WS.                                          IMS0019
CQ9773  COPY CARACTPER-037.WS.                                          IMS0019
CQ9773  COPY CARACTPER-038.WS.                                          IMS0019
CQ9773  COPY CARACTPER-039.WS.                                          IMS0019
CQ9773  COPY CARACTPER-040.WS.                                          IMS0019
R11058  COPY CARACTPER-041.WS.                                          STT0021
R12051  COPY CARACTPER-042.WS.                                          STT0020
R12051  COPY CARACTPER-043.WS.                                          STT0020
R12212  COPY CARACTPER-045.WS.                                          IMS0022
