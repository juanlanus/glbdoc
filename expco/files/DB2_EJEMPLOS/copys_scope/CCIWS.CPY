       01  Constantes.
           03  Publicname              PIC X(30)    VALUE SPACES.
           03  CCI-Module              PIC X(8)    VALUE "CCINAMPU".
           03  Machinename             PIC X(21)   VALUE SPACES.

       01  Otros.
           03 Var-Ambiente             PIC X(80).

       01  Apuntadores-Memoria.
           03  Mem-Ptr                 USAGE POINTER.
           03  Mem-Size                PIC X(4)    COMP-5 VALUE 2050.
           03  Mem-Flags               PIC X(4)    COMP-5 VALUE 2.

       01  Parametros-CCI.
           03  Srvrhandle              PIC X(4)    COMP-5 VALUE 0.
           03  Maxlen                  PIC X(4)    COMP-5.
           03  Recvlen                 PIC X(4)    COMP-5.
           03  Actuallen               PIC X(4)    COMP-5.
           03  Sendlen                 PIC X(4)    COMP-5.
           03  Sessid                  PIC X(4)    COMP-5 VALUE 0.
           03  Async                   PIC S9(9)   COMP-5 VALUE 0.
           03  Cciend                  PIC X(4)    COMP-5 VALUE 0.

        01 Signature-Block.
           03  Signature.
               05  CCIsign             PIC X(6).
               05  CCItype             PIC X(8).
               05  CCIversn            PIC X(6).
           03  CCIcaltab               USAGE POINTER.

