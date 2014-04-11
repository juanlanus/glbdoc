REMARK*CARACTPER-039.WS.v.0001.
REMARK******************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA 039            *
REMARK******************************************************************
      * IMS0001     | R11027 - VERSION INICIAL CARACTERISTICAS         *
      * 11-ABR-2013 |          PERSONALIZADAS PARA ALCA.               *
      * M.SALAMANCA |                                     REQ. CQ11027 *
      ******************************************************************
        01 PERSONALIZACION-39.
              
           02 PER-39-NIVEL-00-INQ.
              03 00INQ999_039           PIC 9(04).
              
           02 PER-39-NIVEL-00-END.
              03 00END999RO039          PIC 99999999.99.
              03 00END998RO039          PIC 99999999.99.
              
           02 PER-39-NIVEL-00-NUM.
              03 00NUM999TO039          PIC 9(04).
              03 00NUM998TO039          PIC 9(04).
              
           02 PER-39-NIVEL-01-INQ.
              03 01INQ999_039           PIC 9(04).
              
           02 PER-39-NIVEL-01-END.
              03 01END999RO039          PIC 99999999.99.
              03 01END998RO039          PIC 99999999.99.
              
           02 PER-39-NIVEL-02-NUM.
              03 02NUM999TO039          PIC 9(04).
              03 02NUM998TO039          PIC 9(04).
