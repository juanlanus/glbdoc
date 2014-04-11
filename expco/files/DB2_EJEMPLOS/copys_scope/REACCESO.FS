      * REACCESO-FS.v.0002.
      * ARCHIVO QUE CONTIENE LAS IDENTIFICACIONES QUE FALLARON POR LO MENOS DOS
      * PREGUNTAS DE UN CUESTIONARIO EN UN DIA PARA PODER CALCULAR EL REACCESO 
      * PARA LA RED NEURONAL
      ******************************************************************FARF002
      * FARF002     * BLQREA - Se ajusta para que no genere un 65 File  FARF002
      * FRANKLIN A.R* locked en el archivo de readceso                  FARF002
      * MAY 25-2012 *                                                   FARF002
      ******************************************************************FARF002
      *
           SELECT REACCESO
                  ASSIGN TO DISK TITLE-REACCESO
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-REACCESO
BLQREA            LOCK MODE IS MANUAL WITH LOCK ON RECORD.              FARF002           
