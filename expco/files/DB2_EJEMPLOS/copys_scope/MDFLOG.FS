REMARK* MDFLOG-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO LOG DEL PROGRAMA ICGMDF                                *
REMARK******************************************************************
           SELECT MDFLOG
                  ASSIGN  TO DISK TITLE-MDFLOG
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS FILE-STATUS-MDFLOG
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
