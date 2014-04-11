      * ICBPER.v.02.003
      * MAESTRO DE CIUDADANOS NACIONALES Y EXTRANJERAS Y DE PERSONAS
      * JURIDICAS QUE TIENEN INFORMACION EN LAS BASE DE DATOS DE 
      * DATACREDITO- ICMPER
      *************************************************************
      * FAC0003    * MODPER * ARCHIVO SE DEFINE COMO FORMATO TIPO 8 
      * MAY.26/2003         *
      * F.CUADROS           *
      *************************************************************
      
MODPER $SET IDXFORMAT"8"                                                FAC0003
           SELECT MAESTRO-PERSON
                  ASSIGN TO DISK MAES-PERSON-TITLE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MAES-PERSON
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD.
MODPER $SET IDXFORMAT"0"                                                FAC0003
