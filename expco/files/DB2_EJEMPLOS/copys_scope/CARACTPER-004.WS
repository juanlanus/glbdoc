REMARK*CARACTPER-004.WS.v.0004.
REMARK******************************************************************
REMARK* CAV0002     | CQ5341 - SE INCLUYE LAS CARACTERSITICAS DEFINIDAS*
REMARK* 28-MAR-2012 |          EN LA SOLICITUD 5341.                   *
REMARK* 18-MAY-2012 | CQ5926 - INCLUSION ARCHIVO RETENCION CUPOS       *
REMARK******************************************************************
REMARK* EHA0003     | CQ6105 - SE INCLUYE LAS CARACTERSITICAS DEFINIDAS*
REMARK* 29-MAY-2012 |          EN LA SOLICITUD 6105.                   *
REMARK******************************************************************
REMARK* IMS0004     | CQ7215 - SE INCLUYE ARCHIVO DE PARAMETROS PARA   *
REMARK* 18-SEP-2012 |          PERSONALIZACION DE COLPATRIA.           *
REMARK******************************************************************
        01 PERSONALIZACION-04.                                          
           02 PER-04-NIVEL-01.                                          
             05 01NUM017         PIC 9(4).                              
             05 99END002TO00     PIC X(01).                             
             05 01END999TC004               PIC  9(12) VALUE  ZEROS.    
             05 01END998TC004               PIC  9(12) VALUE  ZEROS.    
             05 99DEM007TO12                PIC  9(06) VALUE  ZEROS.    
             05 99DEM001EX61                PIC  9(06) VALUE  ZEROS.    
             05 99DEM002TO00                PIC  9(06) VALUE  ZEROS.    
             05 99DEM003TO12                PIC  9(06) VALUE  ZEROS.    
             05 99DEM002EX61                PIC  9(06) VALUE  ZEROS.    
             05 99END004TO13                PIC  9(13) VALUE  ZEROS.    
             05 99END020EX61                PIC  9(13) VALUE  ZEROS.    
             05 04END020TO50                PIC  9(13) VALUE  ZEROS.    
             05 99END022EX61                PIC  9(13) VALUE  ZEROS.    
             05 99END019TO13                PIC  9(13) VALUE  ZEROS.    
             05 99EXP003TC00                PIC  9(06) VALUE  ZEROS.    
             05 99EXP003TO07                PIC  9(06) VALUE  ZEROS.    
             05 99MOR001EX61                PIC  9(06) VALUE  ZEROS.    
             05 99MOR001TO00                PIC  9(06) VALUE  ZEROS.    
             05 99MOR002EX61                PIC  9(06) VALUE  ZEROS.    
             05 99MOR002TO00                PIC  9(06) VALUE  ZEROS.    
             05 99MOR003EX61                PIC  9(06) VALUE  ZEROS.    
             05 99MOR007TO12                PIC  9(06) VALUE  ZEROS.    
             05 99MOR008EX61                PIC  9(06) VALUE  ZEROS.    
             05 99MOR008TO12                PIC  9(06) VALUE  ZEROS.    
             05 99MOR009EX61                PIC  9(06) VALUE  ZEROS.    
             05 99MOR009TO12                PIC  9(06) VALUE  ZEROS.    
             05 99MOR016TO12                PIC  9(06) VALUE  ZEROS.    
             05 99MOR028EX61                PIC  9(06) VALUE  ZEROS.    
             05 99MOR028EX04                PIC  9(06) VALUE  ZEROS.    
             05 99NUM005TO00                PIC  9(06) VALUE  ZEROS.    
             05 99MOR010TC00                PIC  9(06) VALUE  ZEROS.    
             05 99DEM004TO00                PIC  9(03) VALUE  ZEROS.    
             05 99DEM001TO00                PIC  9(02) VALUE  ZEROS.    
             05 99DEM006TO00                PIC  9(08) VALUE  ZEROS.    
             05 99DEM008TO00                PIC  9(01) VALUE  ZEROS.    
             05 99MOR004EX61                PIC  9(06)  VALUE ZEROS.    
             05 99MOR005EX61                PIC  9(06)  VALUE ZEROS.    
             05 99MOR006EX61                PIC  9(06)  VALUE ZEROS.    
             05 99MOR007EX61                PIC  9(06)  VALUE ZEROS.    
             05 99MOR011TO12                PIC  9(06)  VALUE ZEROS.    
             05 99MOR013TO12                PIC  9(06)  VALUE ZEROS.    
             05 99MOR014TO12                PIC  9(06)  VALUE ZEROS.    
             05 99MOR015TO12                PIC  9(06)  VALUE ZEROS.    
             05 99EXP002EX61                PIC  9(06)  VALUE ZEROS.    
             05 99EXP045EX04                PIC  9(06)  VALUE ZEROS.    
             05 MORAS30-99EXP002EX61        PIC  9(06)  VALUE ZEROS.    
             05 MORAS60-99EXP002EX61        PIC  9(06)  VALUE ZEROS.    
             05 MORAS90-99EXP002EX61        PIC  9(06)  VALUE ZEROS.    
             05 MORAS120-99EXP002EX61       PIC  9(06)  VALUE ZEROS.    
             05 SW-NODESEADOS-COLP          PIC  9(01)  VALUE ZEROS.    
CQ5341       05 01END999I1004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 01END999RO004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 01END998I1004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 01END998RO004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 01END998MX004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 02END999TO004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ6105       05 SUMA-CUPOS-TDC-61           PIC  9(11) VALUE  ZEROS.    EHA0003
CQ6105       05 SUMA-CUPOS-CBR-61           PIC  9(11) VALUE  ZEROS.    EHA0003
CQ6105       05 SUMA-CUPOS-OTR-61           PIC  9(11) VALUE  ZEROS.    EHA0003
CQ6105       05 SW-INDICADOR-BUI            PIC  X(01) VALUE  SPACES.   EHA0003
      *                                                                 
             05 W-FECHA-SUP                 PIC  9(08) VALUE  ZEROS.    
             05 IND-E                       PIC  9(05) VALUE  ZEROS.    
CQ5341     02 PER-04-NIVEL-00.                                          CAV0002
CQ5341       05 00END999I1004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 00END999RO004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 00END998I1004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 00END998RO004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 00END998MX004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5341       05 00END999TO004               PIC  9(11) VALUE  ZEROS.    CAV0002
CQ5926*
CQ5926     02 PER-04-EXTERNAS.                                          CAV0002
CQ5926       05 PER04-IND-REG-ENC           PIC  9(01) VALUE  ZEROS.    CAV0002
CQ5926       05 PER04-AUMENTO-CUPO          PIC  999.99 VALUE ZEROS.    CAV0002
CQ5926       05 PER04-AUMENTO-CUPO2         PIC  999.99 VALUE ZEROS.    CAV0002

CQ7215     02  TMP-LLAVE-D61.                                           IMS0004
CQ7215       05  TMP-CAMPO-D61            PIC  9(02).                   IMS0004
CQ7215       05  TMP-POSI-D61             PIC  9(02).                   IMS0004
CQ7215       05  TMP-MODELO-D61           PIC  9(04).                   IMS0004
CQ7215     02  TMP-VALOR-D61              PIC  9(03)V9(02).             IMS0004
CQ7215     02                      REDEFINES  TMP-VALOR-D61.            IMS0004
CQ7215       05  TMP-VAL-INT-D61          PIC  9(01)V9(04).             IMS0004
