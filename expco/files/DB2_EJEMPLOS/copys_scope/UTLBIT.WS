******* UTLBIT-WS.v.0001
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES
******* QUE UTILIZAN LAS RUTINAS DE BASE DE LA RUTINA DE BITS      .
***********************************************************************
UTLCRI 01 VARIABLES-UTLBIT.                                      
UTLCRI    03 I-UTLBIT             PIC 9(5).
UTLCRI    03 POSCAR-UTLBIT        PIC 9(5).
UTLCRI    03 POSFLA-UTLBIT        PIC 9(5).
UTLCRI    03 INPUT-STR-UTLBIT     PIC X(100).
UTLCRI    03 INPUT-LEN-UTLBIT     PIC 9(03).
UTLCRI    03 INPUT-FLA-UTLBIT     PIC 9(03).
UTLCRI    03 INPUT-VAL-UTLBIT     PIC 9(01).
UTLCRI    03 OUTPUT-ERR-UTLBIT    PIC 9(01).
UTLCRI    03 OUTPUT-VAL-UTLBIT    PIC 9(01).
UTLCRI    03 VALOR-UTLBIT         PIC 9(05).
UTLCRI    03 ASCII-UTLBIT         PIC 9(05).
UTLCRI    03 ASCXX-UTLBIT         PIC 9(05).
UTLCRI    03 CAR-UTLBIT           PIC X(01).
UTLCRI 01 CARACTERES-UTLBIT.
UTLCRI    03 FILLER PIC X(16) VALUE X'000102030405060708090A0B0C0D0E0F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'101112131415161718191A1B1C1D1E1F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'202122232425262728292A2B2C2D2E2F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'303132333435363738393A3B3C3D3E3F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'404142434445464748494A4B4C4D4E4F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'505152535455565758595A5B5C5D5E5F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'606162636465666768696A6B6C6D6E6F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'707172737475767778797A7B7C7D7E7F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'808182838485868788898A8B8C8D8E8F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'909192939495969798999A9B9C9D9E9F'.
UTLCRI    03 FILLER PIC X(16) VALUE X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'.
UTLCRI    03 FILLER PIC X(16) VALUE X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'.
UTLCRI    03 FILLER PIC X(16) VALUE X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'.
UTLCRI    03 FILLER PIC X(16) VALUE X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'.
UTLCRI    03 FILLER PIC X(16) VALUE X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'.
UTLCRI    03 FILLER PIC X(16) VALUE X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'.
UTLCRI 01 FILLER REDEFINES CARACTERES-UTLBIT.
ULTCRI    03 V-UTLBIT OCCURS 256 TIMES PIC X.
