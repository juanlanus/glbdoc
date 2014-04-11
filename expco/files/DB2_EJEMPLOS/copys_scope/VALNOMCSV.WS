REMARK* VALNOMCSV-WS.v.0001
REMARK******************************************************************
REMARK*  DEFINICION DE LOS CAMPOS UTILIZADOS EN LA RUTINA VALNOMCSV.PROC
REMARK******************************************************************
       01 VALIABLES-VALNOMCSV.
          03 TIPO-ID-VALNOM-CSV             PIC 9(01).
          03 NRO-ID-VALNOM-CSV              PIC 9(11).
          03 NOMBRE-VALNOM-CSV              PIC X(45).
          03 VECTOR-ENT-VALNOM-CSV.
             04 BLQ-ENT-VALNOM-CSV     OCCURS 20 TIMES.
                05 BLQI-ENT-VALNOM     OCCURS 15 TIMES PIC X.
          03 CADENA-VALNOM-CSV.
             04 FILLER                      PIC X(2) VALUE "B ".
             04 NOMBRE-AUX-VALNOM-CSV       PIC X(45).
          03 I-CSV                          PIC 9(05).
          03 J-CSV                          PIC 9(05).
          03 SW-VALNOM-CSV                  PIC 9(01).
          03 TOT-BLOQUES-CSV                PIC 9(05).
          03 TOT-BLOQUOK-CVS                PIC 9(05).
          03 TOT-NUMER-CSV                  PIC 9(05).
          03 TOT-CARCONT-CSV                PIC 9(02).
          03 TOT-VOCCONT-CSV                PIC 9(02).
          03 SW-SIN-VOCAL-CSV               PIC 9(05).
          03 BASURA-VALNOM-CSV              PIC X(10).
          03 VECTOR-CARATXBLQ-CSV.
             04 VECTOR-CARAXBLQ-CSV    OCCURS 20 TIMES.
                05 TOT-CARAXBLQ-CSV         PIC 9(02).
                05 TOT-VOCALXBLQ-CSV        PIC 9(02).
                05 IND-BLOQCSV-OK           PIC 9(01).
