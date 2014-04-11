REMARK* SCOVEXC-FS.v.0001
REMARK******************************************************************
REMARK* ARCHIVO DONDE QUEDAN REPORTADAS LAS TARJETAS A LAS CUALES NO SE*
REMARK* LES GENERO EXTRACUPO CON SU CODIGO ASOCIADO (VER SCOVEXC.FD)   *
REMARK******************************************************************
           SELECT SCOVEXC
                  ASSIGN  TO DISK TITLE-SCOVEXC
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  LOCK MODE    IS MANUAL WITH LOCK ON RECORD.
