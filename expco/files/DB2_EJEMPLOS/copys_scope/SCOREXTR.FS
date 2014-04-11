REMARK* SCOREXTR-FS.v.0002
REMARK******************************************************************
REMARK* ARCHIVO QUE ALMACENA  LA INFORMACION DE DE LOS SCORES 
REMARK* EXTERNOS ASOCIADOS A CADA CLIENTE POR NIT
REMARK* CAV002 | LOCREC : SE ADICIONA EN LOCK RECORD
REMARK******************************************************************
           SELECT SCOREXTR ASSIGN TO DISK "$DATABASE/SCOREXTR.DAT"        
                           ORGANIZATION IS LINE SEQUENTIAL        
LOCREC                     ACCESS MODE IS SEQUENTIAL                    CAV002 
LOCREC                     LOCK MODE IS MANUAL WITH LOCK ON RECORD.     CAV002 
