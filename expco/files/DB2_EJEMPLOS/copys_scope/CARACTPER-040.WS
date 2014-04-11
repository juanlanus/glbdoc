REMARK*CARACTPER-040.WS.v.0001.
REMARK******************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA SERFINANSA        
REMARK******************************************************************
REMARK* IMS0001    | R11508 - SE AGREGAN NUEVAS CARACTERISTICAS        *
REMARK* 04-ABR-2013|                                                   *
REMARK* M.SALAMANCA|                                           CQ11508 *
REMARK******************************************************************
                                                                        
        01 VARIABLES-PER-040.                                           
           03 ROFLAG-040                PIC 9(03)  VALUE ZEROS.         
           03 RODFLAG-040-NUM           PIC 9(03)  VALUE ZEROS.         
           03 ROVFLAG-040-NUM           PIC 9(03)  VALUE ZEROS.         
           03 ROVFLAG-040-3             PIC 9(03)  VALUE ZEROS.         
           03 ROVFLAG-040-6             PIC 9(03)  VALUE ZEROS.         
           03 ROVFLAG-040-12            PIC 9(03)  VALUE ZEROS.         
           03 ROVFLAG-040-C             PIC 9(03)  VALUE ZEROS.         
      *                                                                 
           03 RO-01FLAG-040-N           PIC 9(03)  VALUE ZEROS.         
           03 RO-01FLAG-040-J           PIC 9(03)  VALUE ZEROS.         
           03 RO-01OPFLAG-040-N         PIC 9(03)  VALUE ZEROS.         
           03 RO-01OPFLAG-040-NE        PIC 9(03)  VALUE ZEROS.         
           03 RO-01OPFLAG-040-J         PIC 9(03)  VALUE ZEROS.         
           03 RO-01DFLAG-040-N          PIC 9(03)  VALUE ZEROS.         
           03 RO-01DFLAG-040-N-END      PIC 9(03)  VALUE ZEROS.         
           03 RO-01DFLAG-040-J-END      PIC 9(03)  VALUE ZEROS.         
           03 RO-01VFLAG-040-N-END      PIC 9(03)  VALUE ZEROS.         
           03 RO-01VFLAG-040-J-END      PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-N          PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-INI-N      PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-SDO-N      PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-CUO-N      PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-MOR-N      PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-J          PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-INI-J      PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-SDO-J      PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-CUO-J      PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-MOR-J      PIC 9(03)  VALUE ZEROS.         
           03 RO-01FFLAG-040-991-N      PIC 9(03)  VALUE ZEROS.         
           03 UTILIZ-SRFNSC             PIC 9(03)V9(04) VALUE ZEROS.    
           03 I-IND-SRF                 PIC 9(03)  VALUE ZEROS.         
           03 W-SUSC-SRFN.                                              
              05 W-SUS-SRFN-T           PIC 9(01)  VALUE ZEROS.         
              05 W-SUS-SRFN-S           PIC 9(06)  VALUE ZEROS.         
      *                                                                 
           02 PER-40-COD-SUS-SRF       PIC 999999 VALUE ZEROS.          
              88 COD-SUS-SRF         VALUE 020337 140301 180037 180137  
                                           180337 180437 180637 180937  
                                           181437 460040.               
                                                                        
       01  ENT-SERFINANSA               PIC X(02)  VALUE SPACES.        
       01  SW-SUSCR-SRFN                PIC 9(01)  VALUE ZEROS.         
                                                                        
        01 PORTAF-PER-040.                                              
           03 COD-CTA-PORTAF-040       PIC X(05)   VALUE SPACES.        
                                                                        
        01 PERSONALIZACION-40.                                          
           02 PER-40-NIVEL-01-NUM.                                                                                                       
              03 01NUM999XX040.                                         
                 04 01NUM999RO040       PIC 9(03).                      
              03 01NUM998XX040.                                         
                 04 01NUM998RO040       PIC 9(03).                      
              03 01NUM997XX040.                                         
                 04 01NUM997RO040       PIC 9(03).                      
      *                                                                 
           02 PER-40-NIVEL-01-END.                                      
              03 01END999XX040.                                         
                 04 01END999RO040       PIC 9(11).                      
              03 01END998XX040.                                         
                 04 01END998RO040       PIC 9(11).                      
              03 01END997XX040.                                         
                 04 01END997RO040       PIC 9(11).                      
              03 01END996XX040.                                         
                 04 01END996RO040       PIC 9(11).                      
              03 01END995XX040.                                         
                 04 01END995RO040       PIC 9(11).                      
              03 01END994XX040.                                         
                 04 01END994RO040       PIC 9(11).                      
              03 01END993XX040.                                         
                 04 01END993RO040       PIC 9(11).                      
              03 01END992XX040.                                         
                 04 01END992RO040       PIC 9(11).                      
              03 01END991XX040.                                         
                 04 01END991RO040       PIC 9(11).                      
              03 01END990XX040.                                         
                 04 01END990RO040       PIC 9(11).                      
              03 01END989XX040.                                         
                 04 01END989RO040       PIC 9(03).                      
                                                                        
           02 PER-40-NIVEL-01-EXP.                                      
              03 01EXP999RO040          PIC 9(04).                      
              03 01EXP998RO040          PIC 9(04).                      
              03 01EXP997RO040          PIC 9(04).                      
              03 01EXP996RO040          PIC 9(04).                      
                                                                        
           02 PER-40-NIVEL-00-EXP.                                      
              03 00EXP999RO040          PIC 9(04).                      
              03 00EXP998RO040          PIC 9(04).                      
              03 00EXP997RO040          PIC 9(04).                      
              03 00EXP996RO040          PIC 9(04).                      
