REMARK*  SCOUSA-WS.V.0010
REMARK*************************************************************************
REMARK* RMB0002     * SCOUHD - SE ADICIONAN LAS VARIABLES NECESARIAS PARA
REMARK* R MARTINEZ  * EL MODELO DE PREDICTA HD  
REMARK* AGOST/2009  * 
REMARK*************************************************************************
REMARK* RMB0003     * SCOHD1 - SE ADICIONAN, MODIFICAN LAS VARIABLES DEL 
REMARK* R MARTINEZ  * SCORE CARD UNO DE PREDICTA HD POR ALINEACION DE ESTE
REMARK* AGOST/2009  * 
REMARK*************************************************************************
REMARK* RMB0004    * OPTI01 : SE OPTIMIZA EL MODELO CON LA INCLUSIN DE
REMARK* 12-MAR-2010*          MORAS DE 120 EN LAS VARIABLES
REMARK**********************************************************************
REMARK* RMB0005    * QUANNW : SE CREAN VARIABLES PARA LA NUEVA VERSION DE
REMARK* 20-NOV-2010*          QUANTO                                
REMARK**********************************************************************
REMARK* GJC0006    * EXCL02 : SE CREA VARIABLE PARA PODER CAMBIAR LA
REMARK* 05-DIC-2011*          EXCLUSION 76 DE ACIERTA+
REMARK* G.JIMENEZ  *
REMARK**********************************************************************
REMARK* RMB0007    * ACIEAA : SE CREA VARIABLE PARA EL SCORE DE ADQUISICION
REMARK* 01-JUN-2012*          ACIERTA A
REMARK* R-MARTINEZ *
REMARK**********************************************************************
REMARK* RMB0008    * ACIAA1 : SE CREAN VARIABLES PARA EL SCORE DE ADQUISICION
REMARK* 10-OCT-2012*          ACIERTA A ROTATIVO,INSTALAMENTOS (HIPOTECARIO,
REMARK* R-MARTINEZ *          VEHICULOS), FINANCIERO UNIFICADO
REMARK**********************************************************************
REMARK* RMB0009    * ACIAA2 : SE CREAN VARIABLES PARA EL SCORE DE ADQUISICION
REMARK* 05-DIC-2012*          ACIERTA A COOPERATIVO                           
REMARK* R-MARTINEZ *          
REMARK**********************************************************************
REMARK* RMB0010    * MICROC : SE CREAN VARIABLES PARA EL SCORE DE MICROCRE
REMARK* 19-JUN-2013*          DITO                                  
REMARK**********************************************************************

       01 VARIABLES-CARACT-USA.       
          02 NIVEL00-DEM-USA.
             04 CO00DEM001        PIC X(02).
             04 CO00DEM002        PIC X(02).
             04 CO00DEM003        PIC X(03).
             04 CO00DEM006        PIC X(02).
SCOUHD       04 CO00DEM016        PIC X(02).                            RMB0002
QUANNW       04 CO00DEM026        PIC X(02).                            RMB0005
QUANNW       04 CO00DEM027        PIC X(03).                            RMB0005
QUANNW       04 CO00DEM030        PIC X(04).                            RMB0005
          02 NIVEL01-NUM-USA.
             04 CO01NUM001RO      PIC X(04).
SCOUHD       04 CO01NUM001ROV     PIC 9(04).                            RMB0002
             04 CO01NUM001IN      PIC X(04).
SCOUHD       04 CO01NUM001INV     PIC 9(04).                            RMB0002
             04 CO01NUM001VE      PIC X(04).
SCOUHD       04 CO01NUM001VEV     PIC 9(04).                            RMB0002
             04 CO01NUM001HP      PIC X(04).
SCOUHD       04 CO01NUM001HPV     PIC 9(04).                            RMB0002
             04 CO01NUM001OT      PIC X(04).
SCOUHD       04 CO01NUM001OTV     PIC 9(04).                            RMB0002
             04 CO01NUM001CC      PIC X(04).
SCOUHD       04 CO01NUM001CCV     PIC 9(04).                            RMB0002
             04 CO01NUM001CO      PIC X(04).
SCOUHD       04 CO01NUM001COV     PIC 9(04).                            RMB0002
             04 CO01NUM001AH      PIC X(04).
             04 CO01NUM001CT      PIC X(04).
SCOUHD       04 CO01NUM002CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01NUM002COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01NUM002OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01NUM002OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01NUM002CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01NUM002CCV     PIC 9(04).                            RMB0002
             04 CO01NUM002RO      PIC X(04).
SCOUHD       04 CO01NUM002ROV     PIC 9(04).                            RMB0002
             04 CO01NUM002IN      PIC X(04).
SCOUHD       04 CO01NUM002INV     PIC 9(04).                            RMB0002
             04 CO01NUM002VE      PIC X(04).
SCOUHD       04 CO01NUM002VEV     PIC 9(04).                            RMB0002
             04 CO01NUM002HP      PIC X(04).
SCOUHD       04 CO01NUM002HPV     PIC 9(04).                            RMB0002
QUANNW       04 CO01NUM002CT      PIC X(04).                            RMB0005
QUANNW       04 CO01NUM003AH      PIC X(04).                            RMB0005
QUANNW       04 CO01NUM005CT      PIC X(04).                            RMB0005
OPTI01       04 CO01NUM018CO      PIC X(04).                            RMB0004
SCOUHD       04 CO01NUM021IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01NUM042IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01NUM049CC      PIC X(04).                            RMB0002
ACIEAA       04 CO01NUM050RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01NUM050ROV     PIC 9(04).                            RMB0007
             04 CO01NUM050CC      PIC X(04).
ACIEAA       04 CO01NUM050CCV     PIC 9(04).                            RMB0007
             04 CO01NUM050CT      PIC X(04).
SCOUHD       04 CO01NUM050AH      PIC X(04).                            RMB0002
ACIEAA       04 CO01NUM050AHV     PIC 9(04).                            RMB0007
          02 NIVEL02-NUM-USA.
             04 CO02NUM001TO      PIC X(04).
             04 CO02NUM002TO      PIC X(04).
             04 CO02NUM003TO      PIC X(04).
             04 CO02NUM005TO      PIC X(04).
ACIAA1       04 CO02NUM005TOV     PIC 9(04).                            RMB0008
SCOUHD       04 CO02NUM018TO      PIC X(04).                            RMB0002
SCOUHD       04 CO02NUM018TOV     PIC 9(04).                            RMB0002
SCOUHD       04 CO02NUM020TO      PIC X(04).  *> CARACTERISTICA NUEVA   RMB0002
SCOUHD       04 CO02NUM020TOV     PIC 9(04).  *> NUM DUDOSOS RECAUDO    RMB0002
SCOUHD       04 CO02NUM028TO      PIC X(04).                            RMB0002
ACIEAA       04 CO02NUM029TO      PIC X(04).                            RMB0007
ACIEAA       04 CO02NUM029TOV     PIC 9(04).                            RMB0007
ACIAA2       04 CO02NUM030TO      PIC X(04).                            RMB0009
ACIAA2       04 CO02NUM030TOV     PIC 9(04).                            RMB0009
SCOUHD       04 CO02NUM035TO      PIC X(04).                            RMB0002
QUANNW       04 CO02NUM042CB      PIC X(06).                            RMB0005
QUANNW       04 CO02NUM042CBV     PIC 9(3)V99.                          RMB0005
SCOUHD       04 CO02NUM043CC      PIC X(06).                            RMB0002
SCOUHD       04 CO02NUM043CCV     PIC 9(3)V99.                          RMB0002
SCOUHD       04 CO02NUM043CB      PIC X(06).                            RMB0002
SCOUHD       04 CO02NUM043CBV     PIC 9(3)V99.                          RMB0002
QUANNW       04 CO02NUM043RO      PIC X(06).                            RMB0005
QUANNW       04 CO02NUM043ROV     PIC 9(3)V99.                          RMB0005
             04 CO02NUM080MX      PIC X(06).
             04 CO02NUM080MXV     PIC 9(3)V99.
             04 CO02NUM086MX      PIC X(06).
             04 CO02NUM086MXV     PIC 9(3)V99.
          02 NIVEL01-END-USA.
             04 CO01END001RO      PIC X(11).    
             04 CO01END001ROV     PIC 9(8)V99.  
             04 CO01END001IN      PIC X(11).    
             04 CO01END001INV     PIC 9(8)V99.  
             04 CO01END001VE      PIC X(11).    
             04 CO01END001VEV     PIC 9(8)V99.  
             04 CO01END001HP      PIC X(11).    
             04 CO01END001HPV     PIC 9(8)V99.  
             04 CO01END001OT      PIC X(11).    
             04 CO01END001OTV     PIC 9(8)V99.  
SCOUHD       04 CO01END003RO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END003ROV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END003IN      PIC X(11).                            RMB0002
SCOUHD       04 CO01END003INV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END003VE      PIC X(11).                            RMB0002
SCOUHD       04 CO01END003VEV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END003HP      PIC X(11).                            RMB0002
SCOUHD       04 CO01END003HPV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END003CC      PIC X(11).                            RMB0002
SCOUHD       04 CO01END003CCV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END003OT      PIC X(11).                            RMB0002
SCOUHD       04 CO01END003OTV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END003CO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END003COV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END004RO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END004ROV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END004IN      PIC X(11).                            RMB0002
SCOUHD       04 CO01END004INV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END004VE      PIC X(11).                            RMB0002
SCOUHD       04 CO01END004VEV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END004HP      PIC X(11).                            RMB0002
SCOUHD       04 CO01END004HPV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END004CC      PIC X(11).                            RMB0002
SCOUHD       04 CO01END004CCV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END004OT      PIC X(11).                            RMB0002
SCOUHD       04 CO01END004OTV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END004CO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END004COV     PIC 9(8)V99.                          RMB0002
             04 CO01END005RO      PIC X(11).    
             04 CO01END005ROV     PIC 9(8)V99.  
SCOUHD       04 CO01END005VE      PIC X(11).                            RMB0002
SCOUHD       04 CO01END005VEV     PIC 9(8)V99.                          RMB0002
QUANNW       04 CO01END005IN      PIC X(11).                            RMB0005
QUANNW       04 CO01END005INV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END005HP      PIC X(11).                            RMB0005
QUANNW       04 CO01END005HPV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END005OT      PIC X(11).                            RMB0005
QUANNW       04 CO01END005OTV     PIC 9(8)V99.                          RMB0005
OPTI01       04 CO01END006RO      PIC X(11).                            RMB0004
OPTI01       04 CO01END006ROV     PIC 9(8)V99.                          RMB0004
             04 CO01END007RO      PIC X(11).    
             04 CO01END007ROV     PIC 9(8)V99.  
             04 CO01END007IN      PIC X(11).    
             04 CO01END007INV     PIC 9(8)V99.  
             04 CO01END007VE      PIC X(11).    
             04 CO01END007VEV     PIC 9(8)V99.  
             04 CO01END007HP      PIC X(11).    
             04 CO01END007HPV     PIC 9(8)V99.  
             04 CO01END007OT      PIC X(11).    
             04 CO01END007OTV     PIC 9(8)V99.  
SCOUHD       04 CO01END007CO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END007COV     PIC 9(8)V99.                          RMB0002
             04 CO01END008RO      PIC X(11).    
             04 CO01END008ROV     PIC 9(8)V99.  
             04 CO01END009RO      PIC X(11).    
             04 CO01END009ROV     PIC 9(8)V99.  
QUANNW       04 CO01END010RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END010ROV     PIC 9(8)V99.                          RMB0005
             04 CO01END011RO      PIC X(11).    
             04 CO01END011ROV     PIC 9(8)V99.  
SCOUHD       04 CO01END012OT      PIC X(11).                            RMB0002
SCOUHD       04 CO01END012OTV     PIC 9(8)V99.                          RMB0002
QUANNW       04 CO01END013RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END013ROV     PIC 9(8)V99.                          RMB0005
ACIAA1       04 CO01END015RO      PIC X(11).                            RMB0008
ACIAA1       04 CO01END015ROV     PIC 9(8)V99.                          RMB0008
ACIAA1       04 CO01END017RO      PIC X(11).                            RMB0008
ACIAA1       04 CO01END017ROV     PIC 9(8)V99.                          RMB0008
ACIAA1       04 CO01END017IN      PIC X(11).                            RMB0008
ACIAA1       04 CO01END017INV     PIC 9(8)V99.                          RMB0008
SCOUHD       04 CO01END017CC      PIC X(11).                            RMB0002
SCOUHD       04 CO01END017CCV     PIC 9(8)V99.                          RMB0002
             04 CO01END018IN      PIC X(11).    
             04 CO01END018INV     PIC 9(8)V99.  
SCOHD1       04 CO01END018CC      PIC X(11).                            RMB0003
SCOHD1       04 CO01END018CCV     PIC 9(8)V99.                          RMB0003
QUANNW       04 CO01END018RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END018ROV     PIC 9(8)V99.                          RMB0005
SCOUHD       04 CO01END020RO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END020ROV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END021CO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END021COV     PIC 9(8)V99.                          RMB0002
QUANNW       04 CO01END021IN      PIC X(11).                            RMB0005
QUANNW       04 CO01END021INV     PIC 9(8)V99.                          RMB0005
SCOUHD       04 CO01END022CO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END022COV     PIC 9(8)V99.                          RMB0002
ACIEAA       04 CO01END022RO      PIC X(11).                            RMB0007
ACIEAA       04 CO01END022ROV     PIC 9(8)V99.                          RMB0007
             04 CO01END023IN      PIC X(11).    
             04 CO01END023INV     PIC 9(8)V99.  
QUANNW       04 CO01END026RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END026ROV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END026IN      PIC X(11).                            RMB0005
QUANNW       04 CO01END026INV     PIC 9(8)V99.                          RMB0005
SCOUHD       04 CO01END027RO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END027ROV     PIC 9(8)V99.                          RMB0002
             04 CO01END027OT      PIC X(11).    
             04 CO01END027OTV     PIC 9(8)V99.  
SCOHD1       04 CO01END027CC      PIC X(11).                            RMB0003
SCOHD1       04 CO01END027CCV     PIC 9(8)V99.                          RMB0003
             04 CO01END028RO      PIC X(11).    
             04 CO01END028ROV     PIC 9(8)V99.  
             04 CO01END028IN      PIC X(11).    
             04 CO01END028INV     PIC 9(8)V99.  
SCOUHD       04 CO01END028OT      PIC X(11).                            RMB0002
SCOUHD       04 CO01END028OTV     PIC 9(8)V99.                          RMB0002
QUANNW       04 CO01END028HP      PIC X(11).                            RMB0005
QUANNW       04 CO01END028HPV     PIC 9(8)V99.                          RMB0005
             04 CO01END030RO      PIC X(11).    
             04 CO01END030ROV     PIC 9(8)V99.  
             04 CO01END031IN      PIC X(11).    
             04 CO01END031INV     PIC 9(8)V99.  
QUANNW       04 CO01END033CC      PIC X(11).                            RMB0005
QUANNW       04 CO01END033CCV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END035IN      PIC X(11).                            RMB0005
QUANNW       04 CO01END035INV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END035RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END035ROV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END035VE      PIC X(11).                            RMB0005
QUANNW       04 CO01END035VEV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END035OT      PIC X(11).                            RMB0005
QUANNW       04 CO01END035OTV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END037RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END037ROV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END038RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END038ROV     PIC 9(8)V99.                          RMB0005
             04 CO01END039RO      PIC X(11).    
             04 CO01END039ROV     PIC 9(8)V99.  
QUANNW       04 CO01END040RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END040ROV     PIC 9(8)V99.                          RMB0005
             04 CO01END047RO      PIC X(11).    
             04 CO01END047ROV     PIC 9(8)V99.  
QUANNW       04 CO01END050RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END050ROV     PIC 9(8)V99.                          RMB0005
             04 CO01END051IN      PIC X(11).    
             04 CO01END051INV     PIC 9(8)V99.  
QUANNW       04 CO01END052IN      PIC X(11).                            RMB0005
QUANNW       04 CO01END052INV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END055IN      PIC X(11).                            RMB0005
QUANNW       04 CO01END055INV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END060RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END060ROV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END068RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END068ROV     PIC 9(8)V99.                          RMB0005
             04 CO01END070RO      PIC X(11).    
             04 CO01END070ROV     PIC 9(8)V99.  
             04 CO01END071RO      PIC X(11).    
             04 CO01END071ROV     PIC 9(8)V99.  
QUANNW       04 CO01END072RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END072ROV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END073RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END073ROV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END075RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END075ROV     PIC 9(8)V99.                          RMB0005
QUANNW       04 CO01END077RO      PIC X(11).                            RMB0005
QUANNW       04 CO01END077ROV     PIC 9(8)V99.                          RMB0005
SCOUHD       04 CO01END080RO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END080ROV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END085RO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END085ROV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO01END087RO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END087ROV     PIC 9(8)V99.                          RMB0002
ACIEAA       04 CO01END088RO      PIC X(11).                            RMB0007
ACIEAA       04 CO01END088ROV     PIC 9(8)V99.                          RMB0007
SCOUHD       04 CO01END089RO      PIC X(11).                            RMB0002
SCOUHD       04 CO01END089ROV     PIC 9(8)V99.                          RMB0002
ACIEAA       04 CO01END092RO      PIC X(11).                            RMB0007
ACIEAA       04 CO01END092ROV     PIC 9(8)V99.                          RMB0007
          02 NIVEL02-END-USA.
SCOUHD       04 CO02END006OT      PIC X(11).                            RMB0002
SCOUHD       04 CO02END006OTV     PIC 9(08)V99.                         RMB0002
QUANNW       04 CO02END007RO      PIC X(11).                            RMB0005
QUANNW       04 CO02END007ROV     PIC 9(08)V99.                         RMB0005
QUANNW       04 CO02END009CB      PIC X(11).                            RMB0005
QUANNW       04 CO02END009CBV     PIC 9(08)V99.                         RMB0005
QUANNW       04 CO02END010RO      PIC X(11).                            RMB0005
QUANNW       04 CO02END010ROV     PIC 9(08)V99.                         RMB0005
QUANNW       04 CO02END010CB      PIC X(11).                            RMB0005
QUANNW       04 CO02END010CBV     PIC 9(08)V99.                         RMB0005
SCOHD1       04 CO02END001CB      PIC X(11).                            RMB0003
SCOHD1       04 CO02END001CBV     PIC 9(08)V99.                         RMB0003
             04 CO02END007CB      PIC X(11).      
             04 CO02END007CBV     PIC 9(08)V99.
ACIEAA       04 CO02END015CB      PIC X(11).                            RMB0007
ACIEAA       04 CO02END015CBV     PIC 9(8)V99.                          RMB0007
SCOUHD       04 CO02END017CB      PIC X(11).                            RMB0002
SCOUHD       04 CO02END017CBV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO02END018CB      PIC X(11).                            RMB0002
SCOUHD       04 CO02END018CBV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO02END019CB      PIC X(11).                            RMB0002
SCOUHD       04 CO02END019CBV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO02END025CB      PIC X(11).                            RMB0002
SCOUHD       04 CO02END025CBV     PIC 9(8)V99.                          RMB0002
SCOUHD       04 CO02END045RO      PIC X(11).                            RMB0002
SCOUHD       04 CO02END045ROV     PIC 9(8)V99.                          RMB0002
          02 NIVEL01-EXP-USA.
             04 CO01EXP001RO      PIC X(03).
ACIEAA       04 CO01EXP001ROV     PIC 9(03).                            RMB0007
SCOUHD       04 CO01EXP001IN      PIC X(03).                            RMB0002
             04 CO01EXP001CC      PIC X(03).
EXCL02       04 CO01EXP001CCV     PIC 9(03).                            GJC0006
EXCL02       04 CO01EXP001OT      PIC X(03).                            GJC0006
EXCL02       04 CO01EXP001OTV     PIC 9(03).                            GJC0006
             04 CO01EXP001CT      PIC X(03).
             04 CO01EXP001VE      PIC X(03).
             04 CO01EXP001AH      PIC X(03).
ACIEAA       04 CO01EXP001AHV     PIC 9(03).                            RMB0007
             04 CO01EXP002RO      PIC X(03).
             04 CO01EXP002HP      PIC X(03).
             04 CO01EXP003RO      PIC X(03).
SCOUHD       04 CO01EXP003OT      PIC X(03).                            RMB0002
SCOUHD       04 CO01EXP004AH      PIC X(03).                            RMB0002
             04 CO01EXP006RO      PIC X(03).
SCOUHD       04 CO01EXP006CC      PIC X(03).                            RMB0002
SCOUHD       04 CO01EXP006CT      PIC X(03).                            RMB0002
SCOUHD       04 CO01EXP007RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01EXP007ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01EXP007IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01EXP007INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01EXP007OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01EXP007OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01EXP007VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01EXP007VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01EXP007CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01EXP007CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01EXP007CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01EXP007COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01EXP007HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01EXP007HPV     PIC 9(04).                            RMB0002
             04 CO01EXP008CO      PIC X(04).
SCOUHD       04 CO01EXP008RO      PIC X(04).                            RMB0002
ACIAA1       04 CO01EXP008ROV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP008IN      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP008INV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP008OT      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP008OTV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP008VE      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP008VEV     PIC 9(04).                            RMB0008
SCOUHD       04 CO01EXP008CC      PIC X(04).                            RMB0002
ACIAA1       04 CO01EXP008CCV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP008HP      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP008HPV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP013RO      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP013ROV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP013IN      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP013INV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP013OT      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP013OTV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP013VE      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP013VEV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP013CC      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP013CCV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01EXP013HP      PIC X(04).                            RMB0008
ACIAA1       04 CO01EXP013HPV     PIC 9(04).                            RMB0008
          02 NIVEL02-EXP-USA.
             04 CO02EXP001TO      PIC X(03).
SCOUHD       04 CO02EXP001TOV     PIC 9(03).                            RMB0002
SCOUHD       04 CO02EXP002TO      PIC X(03).                            RMB0002
SCOUHD       04 CO02EXP002TOV     PIC 9(03).                            RMB0002
SCOHD1       04 CO02EXP003TO      PIC X(03).                            RMB0003
             04 CO02EXP006TO      PIC X(03).
ACIEAA       04 CO02EXP006TOV     PIC 9(03).                            RMB0007
ACIAA1       04 CO02EXP007TO      PIC X(08).                            RMB0008
ACIAA1       04 CO02EXP007TOV     PIC 9(5)V99.                          RMB0008
ACIEAA       04 CO02EXP008TO      PIC X(08).                            RMB0007
ACIEAA       04 CO02EXP008TOV     PIC 9(5)V99.                          RMB0007
SCOUHD       04 CO02EXP008CC      PIC X(08).                            RMB0002
SCOUHD       04 CO02EXP008CCV     PIC 9(5)V99.                          RMB0002
SCOUHD       04 CO02EXP008OT      PIC X(08).                            RMB0002
SCOUHD       04 CO02EXP008OTV     PIC 9(5)V99.                          RMB0002
             04 CO02EXP009TO      PIC X(08).    
             04 CO02EXP009TOV     PIC 9(5)V99.  
             04 CO02EXP010RO      PIC X(08).    
             04 CO02EXP010ROV     PIC 9(5)V99.  
QUANNW       04 CO02EXP012TO      PIC X(08).                            RMB0005
QUANNW       04 CO02EXP012TOV     PIC 9(5)V99.                          RMB0005
QUANNW       04 CO02EXP012RO      PIC X(08).                            RMB0005
QUANNW       04 CO02EXP012ROV     PIC 9(5)V99.                          RMB0005
SCOUHD       04 CO02EXP013TO      PIC X(08).                            RMB0002
SCOUHD       04 CO02EXP013TOV     PIC 9(5)V99.                          RMB0002
          02 NIVEL01-MOR-USA.
SCOUHD       04 CO01MOR001RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR001ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR001IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR001INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR001OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR001OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR001VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR001VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR001CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR001CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR001CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR001COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR001HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR001HPV     PIC 9(04).                            RMB0002
*
ACIEAA       04 CO01MOR002RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR002ROV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR002IN      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR002INV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR002OT      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR002OTV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR002VE      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR002VEV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR002CC      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR002CCV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR002HP      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR002HPV     PIC 9(04).                            RMB0007
*
ACIEAA       04 CO01MOR003RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR003ROV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR003IN      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR003INV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR003OT      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR003OTV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR003VE      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR003VEV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR003CC      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR003CCV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR003HP      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR003HPV     PIC 9(04).                            RMB0007
*
SCOUHD       04 CO01MOR004RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR004ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR004IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR004INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR004OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR004OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR004VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR004VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR004CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR004CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR004CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR004COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR004HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR004HPV     PIC 9(04).                            RMB0002
OPTI01       04 CO01MOR005RO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR005ROV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR005IN      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR005INV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR005OT      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR005OTV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR005CO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR005COV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR005VE      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR005VEV     PIC 9(04).                            RMB0004
SCOHD1       04 CO01MOR005CC      PIC X(04).                            RMB0003
OPTI01       04 CO01MOR005CCV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR005HP      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR005HPV     PIC 9(04).                            RMB0004
ACIEAA       04 CO01MOR006RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR006ROV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR006IN      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR006INV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR006OT      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR006OTV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR006VE      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR006VEV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR006CC      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR006CCV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR006HP      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR006HPV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR007RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR007ROV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR007IN      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR007INV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR007OT      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR007OTV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR007VE      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR007VEV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR007CC      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR007CCV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR007HP      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR007HPV     PIC 9(04).                            RMB0007
SCOUHD       04 CO01MOR008RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR008ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR008IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR008INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR008OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR008OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR008CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR008COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR008VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR008VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR008CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR008CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR008HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR008HPV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR009RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR009ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR009IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR009INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR009OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR009OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR009VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR009VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR009CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR009CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR009CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR009COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR009HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR009HPV     PIC 9(04).                            RMB0002
SCOHD1       04 CO01MOR017CC      PIC X(04).                            RMB0003
SCOHD1       04 CO01MOR022OT      PIC X(04).                            RMB0003
SCOUHD       04 CO01MOR033OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR033CC      PIC X(04).                            RMB0002
ACIEAA       04 CO01MOR033RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR033ROV     PIC 9(04).                            RMB0007
SCOHD1       04 CO01MOR034CC      PIC X(04).                            RMB0003
ACIEAA       04 CO01MOR034CCV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR034RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR034ROV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR034IN      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR034INV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR034OT      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR034OTV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR034VE      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR034VEV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR034HP      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR034HPV     PIC 9(04).                            RMB0007
SCOHD1       04 CO01MOR035OT      PIC X(04).                            RMB0003
OPTI01       04 CO01MOR037RO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR037ROV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR037IN      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR037INV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR037OT      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR037OTV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR037CO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR037COV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR037VE      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR037VEV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR037CC      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR037CCV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR037HP      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR037HPV     PIC 9(04).                            RMB0004
             04 CO01MOR040CC      PIC X(04).
ACIEAA       04 CO01MOR040RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR040ROV     PIC 9(04).                            RMB0007
ACIAA2       04 CO01MOR040IN      PIC X(04).                            RMB0009
ACIAA2       04 CO01MOR040INV     PIC 9(04).                            RMB0009
SCOHD1       04 CO01MOR043OT      PIC X(04).                            RMB0003
OPTI01       04 CO01MOR044RO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR044ROV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR044IN      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR044INV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR044OT      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR044OTV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR044CO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR044COV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR044VE      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR044VEV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR044CC      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR044CCV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR044HP      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR044HPV     PIC 9(04).                            RMB0004
ACIAA1       04 CO01MOR046RO      PIC X(04).                            RMB0008
ACIAA1       04 CO01MOR046ROV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01MOR047RO      PIC X(04).                            RMB0008
ACIAA1       04 CO01MOR047ROV     PIC 9(04).                            RMB0008
OPTI01       04 CO01MOR051RO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR051ROV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR051IN      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR051INV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR051OT      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR051OTV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR051CO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR051COV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR051VE      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR051VEV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR051CC      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR051CCV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR051HP      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR051HPV     PIC 9(04).                            RMB0004
ACIAA1       04 CO01MOR054IN      PIC X(04).                            RMB0008
ACIAA1       04 CO01MOR054INV     PIC 9(04).                            RMB0008
             04 CO01MOR054CC      PIC X(04).
SCOUHD       04 CO01MOR055RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR055ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR055IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR055INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR055OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR055OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR055CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR055COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR055VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR055VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR055CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR055CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR055HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR055HPV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR056RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR056ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR056IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR056INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR056OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR056OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR056CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR056COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR056VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR056VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR056CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR056CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR056HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR056HPV     PIC 9(04).                            RMB0002
OPTI01       04 CO01MOR058RO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR058ROV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR058IN      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR058INV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR058OT      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR058OTV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR058CO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR058COV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR058VE      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR058VEV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR058CC      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR058CCV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR058HP      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR058HPV     PIC 9(04).                            RMB0004
ACIEAA       04 CO01MOR060RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR060ROV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR060IN      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR060INV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR060OT      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR060OTV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR060VE      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR060VEV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR060CC      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR060CCV     PIC 9(04).                            RMB0007
ACIEAA       04 CO01MOR060HP      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR060HPV     PIC 9(04).                            RMB0007
             04 CO01MOR061RO      PIC X(04).
SCOUHD       04 CO01MOR061IN      PIC X(04).                            RMB0002
ACIEAA       04 CO01MOR062RO      PIC X(04).                            RMB0007
ACIEAA       04 CO01MOR062ROV     PIC 9(04).                            RMB0007
OPTI01       04 CO01MOR065RO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR065ROV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR065IN      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR065INV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR065OT      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR065OTV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR065CO      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR065COV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR065VE      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR065VEV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR065CC      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR065CCV     PIC 9(04).                            RMB0004
OPTI01       04 CO01MOR065HP      PIC X(04).                            RMB0004
OPTI01       04 CO01MOR065HPV     PIC 9(04).                            RMB0004
ACIAA1       04 CO01MOR067RO      PIC X(04).                            RMB0008
ACIAA1       04 CO01MOR067ROV     PIC 9(04).                            RMB0008
ACIAA1       04 CO01MOR068RO      PIC X(04).                            RMB0008
ACIAA1       04 CO01MOR068ROV     PIC 9(04).                            RMB0008
SCOUHD       04 CO01MOR068IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR071RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR071ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR071IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR071INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR071OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR071OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR071CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR071COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR071VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR071VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR071CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR071CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR071HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR071HPV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR072RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR072ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR072IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR072INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR072OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR072OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR072CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR072COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR072VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR072VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR072CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR072CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR072HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR072HPV     PIC 9(04).                            RMB0002
ACIAA2       04 CO01MOR074RO      PIC X(04).                            RMB0009
ACIAA2       04 CO01MOR074ROV     PIC 9(04).                            RMB0009
ACIAA2       04 CO01MOR074IN      PIC X(04).                            RMB0009
ACIAA2       04 CO01MOR074INV     PIC 9(04).                            RMB0009
ACIAA2       04 CO01MOR074VE      PIC X(04).                            RMB0009
ACIAA2       04 CO01MOR074VEV     PIC 9(04).                            RMB0009
SCOUHD       04 CO01MOR074HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR074HPV     PIC 9(04).                            RMB0002
ACIAA1       04 CO01MOR074CC      PIC X(04).                            RMB0008
ACIAA1       04 CO01MOR074CCV     PIC 9(04).                            RMB0008
ACIAA2       04 CO01MOR074OT      PIC X(04).                            RMB0009
ACIAA2       04 CO01MOR074OTV     PIC 9(04).                            RMB0009
SCOUHD       04 CO01MOR075RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR075ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR075IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR075INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR075OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR075OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR075CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR075COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR075VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR075VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR075CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR075CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR075HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR075HPV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR076RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR076ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR076IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR076INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR076OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR076OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR076CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR076COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR076VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR076VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR076CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR076CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR076HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR076HPV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR077RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR077ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR077IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR077INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR077OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR077OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR077CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR077COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR077VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR077VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR077CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR077CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR077HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR077HPV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR078RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR078ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR078IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR078INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR078OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR078OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR078CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR078COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR078VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR078VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR078CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR078CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR078HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR078HPV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR079RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR079ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR079IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR079INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR079OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR079OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR079CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR079COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR079VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR079VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR079CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR079CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR079HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01MOR079HPV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01MOR080CC      PIC X(06).                            RMB0002
SCOUHD       04 CO01MOR080CCV     PIC 9(3)V99.                          RMB0002
SCOUHD       04 CO01MOR083CC      PIC X(06).                            RMB0002
SCOUHD       04 CO01MOR083CCV     PIC 9(3)V99.                          RMB0002
SCOHD1       04 CO01MOR090CC      PIC X(06).                            RMB0003
SCOHD1       04 CO01MOR090CCV     PIC 9(3)V99.                          RMB0003
SCOUHD       04 CO01MOR098IN      PIC X(06).                            RMB0002
SCOUHD       04 CO01MOR098INV     PIC 9(3)V99.                          RMB0002
SCOUHD       04 CO01MOR101OT      PIC X(06).                            RMB0002
SCOUHD       04 CO01MOR101OTV     PIC 9(3)V99.                          RMB0002
          02 NIVEL01-INQ-USA.
             04 CO01INQ002        PIC X(04).
ACIAA1       04 CO01INQ016        PIC X(04).                            RMB0008
ACIAA1       04 CO01INQ016V       PIC 9(04).                            RMB0008
ACIAA1       04 CO01INQ017        PIC X(04).                            RMB0008
ACIAA1       04 CO01INQ017V       PIC 9(04).                            RMB0008
ACIAA1       04 CO01INQ025        PIC X(04).                            RMB0008
ACIAA1       04 CO01INQ025V       PIC 9(04).                            RMB0008
          02 NIVEL01-ACP-USA.
SCOUHD       04 CO01ACP001RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP002RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP002ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP002IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP002INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP002OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP002OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP002VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP002VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP002CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP002CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP002CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP002COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP002HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP002HPV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP003RO      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP003ROV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP003IN      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP003INV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP003OT      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP003OTV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP003VE      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP003VEV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP003CC      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP003CCV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP003CO      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP003COV     PIC 9(04).                            RMB0002
SCOUHD       04 CO01ACP003HP      PIC X(04).                            RMB0002
SCOUHD       04 CO01ACP003HPV     PIC 9(04).                            RMB0002
ACIAA1       04 CO01ACP004RO      PIC X(04).                            RMB0008
ACIAA1       04 CO01ACP004ROV     PIC 9(04).                            RMB0008
SCOUHD       04 CO01ACP005RO      PIC X(04).                            RMB0002
             04 CO01ACP006IN      PIC X(04).
             04 CO01ACP008CT      PIC X(04).
             04 CO01ACP008CC      PIC X(04).
             04 CO01ACP007CT      PIC X(04).
ACIAA1       04 CO01ACP007CTV     PIC 9(04).                            RMB0008
SCOUHD       04 CO01ACP009OT      PIC X(04).                            RMB0002
ACIAA2       04 CO01ACP015IN      PIC X(04).                            RMB0009
ACIAA2       04 CO01ACP015INV     PIC 9(04).                            RMB0009
SCOUHD    02 NIVEL02-ACP-USA.                                           RMB0002
SCOUHD       04 CO02ACP002        PIC X(04).                            RMB0002
REMARK* SE INCLUYE COPY PROPIO PARA EL MANEJO DE PREDICTOR DE INGRESOS
REMARK* QUE AHORA SE LLAMA QUANTO
       COPY SCOUSAPI.WS.
SCOUHD COPY SCOUSAHD.WS.                                                RMB0002 
ACIEAA COPY SCOUSAAA.WS.                                                RMB0007
MICROC COPY SCODAMIC.WS.                                                RMB0010
