      *  EST001.v.02.002
      *  ARCHIVO QUE CONTIEN EL MAYOR NUMERO DE CEDULA GRABADO EN CADA 
      *  UNO DE LOS CUATRO ARCHIVOS FISICOS QUE FORMAN EL ARCHIVO DE LA
      *  REGISTRADURIA
002200*
001800     SELECT CEDULAS                                               GRB
001900            ASSIGN TO DISK "CEDULAS"                              GRB
002000            ORGANIZATION LINE SEQUENTIAL                          GRB
002100            ACCESS MODE IS SEQUENTIAL.                            GRB
002200*
002300     SELECT FILERROR                                              GRB
002400            ASSIGN TO DISK "ERROR_ICBNAP"                         GRB
002500            ORGANIZATION LINE SEQUENTIAL                          GRB
002600            ACCESS MODE IS SEQUENTIAL.                            GRB
