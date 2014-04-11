       01  Constantes-dian.
           03  Publicname-dian         PIC X(30)   VALUE SPACES.
           03  CCI-Module-dian         PIC X(8)    VALUE "CCINAMPU".
           03  Machinename-dian        PIC X(21)   VALUE SPACES.

       01  Otros-dian.
           03 Var-Ambiente-dian        PIC X(80).

       01  Apuntadores-Memoria-dian.
           03  Mem-Ptr-dian            USAGE POINTER.
           03  Mem-Size-dian           PIC X(4)    COMP-5 VALUE 2050.
           03  Mem-Flags-dian          PIC X(4)    COMP-5 VALUE 2.

       01  Parametros-CCI-dian.
           03  Srvrhandle-dian         PIC X(4)    COMP-5 VALUE 0.
           03  Maxlen-dian             PIC X(4)    COMP-5.
           03  Recvlen-dian            PIC X(4)    COMP-5.
           03  Actuallen-dian          PIC X(4)    COMP-5.
           03  Sendlen-dian            PIC X(4)    COMP-5.
           03  Sessid-dian             PIC X(4)    COMP-5 VALUE 0.
           03  Async-dian              PIC S9(9)   COMP-5 VALUE 0.
           03  Cciend-dian             PIC X(4)    COMP-5 VALUE 0.

        01 Signature-Block-dian.
           03  Signature-dian.
               05  CCIsign-dian        PIC X(6).
               05  CCItype-dian        PIC X(8).
               05  CCIversn-dian       PIC X(6).
           03  CCIcaltab-dian          USAGE POINTER.

