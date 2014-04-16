REMARK*CARACTPER-029.WS.v.0002.
REMARK******************************************************************
REMARK*DEFINICION DE CARACTERISTICAS PESONALIZADAS PARA RIPLEY
REMARK******************************************************************
      * IMS0002    | R12047 - Se agregan caracteristicas END, NUM y EXP*
      * 17-MAY-2013|    REQ. CQ. 12047.                                *
      * M.SALAMANCA|                                                   *
      ******************************************************************
        01 PERSONALIZACION-29.
           02 PER-29-NIVEL-00-NUM.
              03 00NUM999TO029          PIC 9(4).
R12047        03 00NUM998RO029          PIC 9(4).                       IMS0002
R12047        03 00NUM997RO029          PIC 9(4).                       IMS0002
R12047        03 00NUM996RO029          PIC 9(4).                       IMS0002
R12047        03 00NUM995TO029          PIC 9(4).                       IMS0002

R12047     02 PER-29-NIVEL-00-END-11.                                   IMS0002
R12047        03 00END999RO029          PIC 9(11).                      IMS0002
R12047        03 00END998RO029          PIC 9(11).                      IMS0002
R12047        03 00END997RO029          PIC 9(11).                      IMS0002
R12047        03 00END996RO029          PIC 9(11).                      IMS0002
R12047        03 00END995RO029          PIC 9(11).                      IMS0002
R12047        03 00END994RO029          PIC 9(11).                      IMS0002
R12047        03 00END993RO029          PIC 9(11).                      IMS0002
R12047        03 00END992RO029          PIC 9(11).                      IMS0002
R12047        03 00END991RO029          PIC 9(11).                      IMS0002
R12047        03 00END990RO029          PIC 9(11).                      IMS0002

R12047     02 PER-29-NIVEL-00-END-04.                                   IMS0002
R12047        03 00END989RO029          PIC 9(4).                       IMS0002

R12047     02 PER-29-NIVEL-00-EXP.                                      IMS0002
R12047        03 00EXP999RO029          PIC 9(4).                       IMS0002
R12047        03 00EXP998RO029          PIC 9(4).                       IMS0002
R12047        03 00EXP997RO029          PIC 9(4).                       IMS0002
R12047        03 00EXP996RO029          PIC 9(4).                       IMS0002
R12047        03 00EXP995TO029          PIC 9(4).                       IMS0002
R12047        03 00EXP994TO029          PIC 9(4).                       IMS0002

           02 PER-29-NIVEL-02-NUM.
              03 02NUM999TO029          PIC 9(4).
R12047        03 01NUM998RO029          PIC 9(4).                       IMS0002
R12047        03 01NUM997RO029          PIC 9(4).                       IMS0002
R12047        03 01NUM996RO029          PIC 9(4).                       IMS0002
R12047        03 02NUM995TO029          PIC 9(4).                       IMS0002

R12047     02 PER-29-NIVEL-01-END-11.                                   IMS0002
R12047        03 01END999RO029          PIC 9(11).                      IMS0002
R12047        03 01END998RO029          PIC 9(11).                      IMS0002
R12047        03 01END997RO029          PIC 9(11).                      IMS0002
R12047        03 01END996RO029          PIC 9(11).                      IMS0002
R12047        03 01END995RO029          PIC 9(11).                      IMS0002
R12047        03 01END994RO029          PIC 9(11).                      IMS0002
R12047        03 01END993RO029          PIC 9(11).                      IMS0002
R12047        03 01END992RO029          PIC 9(11).                      IMS0002
R12047        03 01END991RO029          PIC 9(11).                      IMS0002
R12047        03 01END990RO029          PIC 9(11).                      IMS0002

R12047     02 PER-29-NIVEL-01-END-04.                                   IMS0002
R12047        03 01END989RO029          PIC 9(4).                       IMS0002

R12047     02 PER-29-NIVEL-01-EXP.                                      IMS0002
R12047        03 01EXP999RO029          PIC 9(4).                       IMS0002
R12047        03 01EXP998RO029          PIC 9(4).                       IMS0002
R12047        03 01EXP997RO029          PIC 9(4).                       IMS0002
R12047        03 01EXP996RO029          PIC 9(4).                       IMS0002
R12047        03 02EXP995TO029          PIC 9(4).                       IMS0002
R12047        03 02EXP994TO029          PIC 9(4).                       IMS0002

R12047     02 PER-29-COD-SUS-RPL       PIC 999999 VALUE ZEROS.          IMS0002
R12047        88 COD-SUS-RPL           VALUE 020030.                    IMS0002

R12047     02 PER-29-VAR-TRAB.
R12047        03 UTL-RPL               PIC 9(03)V9(04) VALUE ZEROS.     IMS0002
R12047        03 AUX-EST-ORG           PIC 9(04).                       IMS0002
R12047        03 AUX-FEC-EST-ORG.                                       IMS0002
R12047           05 ANO-FEC-EST-ORG    PIC 9(04).                       IMS0002
R12047           05 MES-FEC-EST-ORG    PIC 9(02).                       IMS0002
