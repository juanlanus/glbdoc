      *BDLCLI-FS.v.0001
      *ARCHIVO DE  CLIENTES DE BASE DE DATOS DE LOCALIZACION CON INDICADOR Y
      *PRIVILEGIO ACTUALIZADO.              
          SELECT BDLCLI
                  ASSIGN TO DISK BDLCLI-TITLE  
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-BDLCLI
                  LOCK MODE IS AUTOMATIC.
