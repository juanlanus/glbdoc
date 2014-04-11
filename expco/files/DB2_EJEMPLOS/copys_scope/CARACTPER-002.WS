REMARK*CARACTPER-002.WS.v.0001.
        01 PERSONALIZACION-02.
           02 PER-02-NIVEL-00.
              03 00MOR129               PIC 9(3).
              03 00MOR130               PIC 9(3).
              03 00MOR131               PIC 9(3).
              03 00MOR132               PIC 9(3).
              03 00MOR133               PIC 9(3).
              03 00MOR134               PIC 9(3).
              03 00MOR135               PIC 9(3).
              03 00MOR136               PIC 9(3).
           02 PER-02-NIVEL-01.
              03 VAR-MOR-PER-02.
                04 V-01MOR-PER-02       OCCURS 9 TIMES.        
                  05 01MOR129       PIC 9(4). *> MORAS  30 ULT 03 MESE 
                  05 01MOR130       PIC 9(4). *> MORAS  60 ULT 03 MESE 
                  05 01MOR131       PIC 9(4). *> MORAS  90 ULT 03 MESE 
                  05 01MOR132       PIC 9(4). *> MORAS 120 ULT 03 MESE 
                  05 01MOR133       PIC 9(4). *> MORAS  30 ULT 09 MESE 
                  05 01MOR134       PIC 9(4). *> MORAS  60 ULT 09 MESE 
                  05 01MOR135       PIC 9(4). *> MORAS  90 ULT 09 MESE 
                  05 01MOR136       PIC 9(4). *> MORAS 120 ULT 09 MESE 
             03 FILLER REDEFINES VAR-MOR-PER-02.
      *                                                                 
             05 01MOR129RO       PIC 9(4).                              
             05 01MOR130RO       PIC 9(4).                              
             05 01MOR131RO       PIC 9(4).                              
             05 01MOR132RO       PIC 9(4).                              
             05 01MOR133RO       PIC 9(4).                              
             05 01MOR134RO       PIC 9(4).                              
             05 01MOR135RO       PIC 9(4).                              
             05 01MOR136RO       PIC 9(4).                              
      *                                                                 
             05 01MOR129IN       PIC 9(4).                              
             05 01MOR130IN       PIC 9(4).                              
             05 01MOR131IN       PIC 9(4).                              
             05 01MOR132IN       PIC 9(4).                              
             05 01MOR133IN       PIC 9(4).                              
             05 01MOR134IN       PIC 9(4).                              
             05 01MOR135IN       PIC 9(4).                              
             05 01MOR136IN       PIC 9(4).                              
      *                                                                 
             05 01MOR129VE       PIC 9(4).                              
             05 01MOR130VE       PIC 9(4).                              
             05 01MOR131VE       PIC 9(4).                              
             05 01MOR132VE       PIC 9(4).                              
             05 01MOR133VE       PIC 9(4).                              
             05 01MOR134VE       PIC 9(4).                              
             05 01MOR135VE       PIC 9(4).                              
             05 01MOR136VE       PIC 9(4).                              
      *                                                                 
             05 01MOR129HP       PIC 9(4).                              
             05 01MOR130HP       PIC 9(4).                              
             05 01MOR131HP       PIC 9(4).                              
             05 01MOR132HP       PIC 9(4).                              
             05 01MOR133HP       PIC 9(4).                              
             05 01MOR134HP       PIC 9(4).                              
             05 01MOR135HP       PIC 9(4).                              
             05 01MOR136HP       PIC 9(4).                              
      *                                                                 
             05 01MOR129OT       PIC 9(4).                              
             05 01MOR130OT       PIC 9(4).                              
             05 01MOR131OT       PIC 9(4).                              
             05 01MOR132OT       PIC 9(4).                              
             05 01MOR133OT       PIC 9(4).                              
             05 01MOR134OT       PIC 9(4).                              
             05 01MOR135OT       PIC 9(4).                              
             05 01MOR136OT       PIC 9(4).                              
      *                                                                 
             05 01MOR129CC       PIC 9(4).                              
             05 01MOR130CC       PIC 9(4).                              
             05 01MOR131CC       PIC 9(4).                              
             05 01MOR132CC       PIC 9(4).                              
             05 01MOR133CC       PIC 9(4).                              
             05 01MOR134CC       PIC 9(4).                              
             05 01MOR135CC       PIC 9(4).                              
             05 01MOR136CC       PIC 9(4).                              
      *                                                                 
             05 01MOR129CO       PIC 9(4).                              
             05 01MOR130CO       PIC 9(4).                              
             05 01MOR131CO       PIC 9(4).                              
             05 01MOR132CO       PIC 9(4).                              
             05 01MOR133CO       PIC 9(4).                              
             05 01MOR134CO       PIC 9(4).                              
             05 01MOR135CO       PIC 9(4).                              
             05 01MOR136CO       PIC 9(4).                              
      *                                                                 
             05 01MOR129AH       PIC 9(4).                              
             05 01MOR130AH       PIC 9(4).                              
             05 01MOR131AH       PIC 9(4).                              
             05 01MOR132AH       PIC 9(4).                              
             05 01MOR133AH       PIC 9(4).                              
             05 01MOR134AH       PIC 9(4).                              
             05 01MOR135AH       PIC 9(4).                              
             05 01MOR136AH       PIC 9(4).                              
      *                                                                 
             05 01MOR129CT       PIC 9(4).                              
             05 01MOR130CT       PIC 9(4).                              
             05 01MOR131CT       PIC 9(4).                              
             05 01MOR132CT       PIC 9(4).                              
             05 01MOR133CT       PIC 9(4).                              
             05 01MOR134CT       PIC 9(4).                              
             05 01MOR135CT       PIC 9(4).                              
             05 01MOR136CT       PIC 9(4).                              
           02 PER-02-NIVEL-02.
             05 02MOR129TO       PIC 9(4).                              
             05 02MOR130TO       PIC 9(4).                              
             05 02MOR131TO       PIC 9(4).                              
             05 02MOR132TO       PIC 9(4).                              
             05 02MOR133TO       PIC 9(4).                              
             05 02MOR134TO       PIC 9(4).                              
             05 02MOR135TO       PIC 9(4).                              
             05 02MOR136TO       PIC 9(4).
