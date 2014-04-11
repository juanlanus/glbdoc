      * BDIIMREG.v.03.0001
      *===============================================================
      * ESTRUCTURA DE ARCHIVO DE INDICADORES DE MODIFICACIÓN PARA LA
      * REGISTRADURÍA
      *===============================================================
           SELECT BDIIMREG 
                  ASSIGN TO DISK "$DATABASE/BDIIMREG.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
