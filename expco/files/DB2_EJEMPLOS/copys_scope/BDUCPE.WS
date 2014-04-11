******* BDUCPE-WS.v.02.0002
******* DESCRIPCION DE CADA UNO DE LOS CAMPOS BASICOS E INDEPENDIENTES QUE
******* UTILIZAN LAS RUTINAS DE BASE DE DATOS UNIVERSAL CCO PARA PROC. ESPE.
*****************************************************************************
REMARK* VHF0002     * CONLIQ - SE INCLUYEN DOS NUEVOS CAMPOS EN LA BASE*
REMARK* VICTOR HUGO *          DE DATOS: INDICADOR DE LIQUIDACION E    *
REMARK* FORERO      *          INDICADOR DE CONCORDATO                 *
REMARK* OCT.26/2001 * FECVIG - SE PERMITE MANEJAR LA FECHA DE VIGENCIA *
REMARK*             *          EN BLANCO                               *
REMARK******************************************************************
NEWUNI 01 REG-BDUCCO-02.      
NEWUNI    03 CATEGO-BDUCCO        PIC 9(01).   
NEWUNI    03 ORGJUR-BDUCCO        PIC 9(02).   
NEWUNI    03 FECMAT-BDUCCO        PIC 9(08).   
NEWUNI    03 FECREN-BDUCCO        PIC 9(08).   
NEWUNI    03 FECVIG-BDUCCO        PIC 9(08).
FECVIG    03 FECVIG-BDUCCO-R      PIC X(08) REDEFINES FECVIG-BDUCCO.
NEWUNI    03 VCIIU1-BDUCCO        PIC X(07).                                   
NEWUNI    03 VCIIU2-BDUCCO        PIC X(07).                                   
NEWUNI    03 VCIIU3-BDUCCO        PIC X(07).                                   
NEWUNI    03 NUMEMP-BDUCCO        PIC 9(06).                                   
NEWUNI    03 NUMEST-BDUCCO        PIC 9(05).                                   
NEWUNI    03 CAPAUT-BDUCCO        PIC 9(11) COMP-3.                     
NEWUNI    03 CAPPAG-BDUCCO        PIC 9(11) COMP-3.                   
NEWUNI    03 TOTACT-BDUCCO        PIC 9(11) COMP-3.
CONLIQ    03 INDLIQ-BDUCCO        PIC X(01).
CONLIQ    03 INDCON-BDUCCO        PIC X(01).
CONLIQ    03 FILLER               PIC X(01).                   
NEWUNI 01 REG-BDUCCO-03.      
NEWUNI    03 ACTCOR-BDUCCO        PIC 9(11) COMP-3.                     
NEWUNI    03 ACTFIJ-BDUCCO        PIC 9(11) COMP-3.                   
NEWUNI    03 ACTOTR-BDUCCO        PIC 9(11) COMP-3.                   
NEWUNI    03 ACTVAL-BDUCCO        PIC 9(11) COMP-3.                   
NEWUNI    03 PASTOT-BDUCCO        PIC 9(11) COMP-3.                   
NEWUNI    03 PASCOR-BDUCCO        PIC 9(11) COMP-3.                   
NEWUNI    03 PASLAR-BDUCCO        PIC 9(11) COMP-3.                   
NEWUNI    03 PATTOT-BDUCCO        PIC S9(11) COMP-3.                   
NEWUNI    03 PASPAT-BDUCCO        PIC 9(11) COMP-3.                   
NEWUNI    03 VENNET-BDUCCO        PIC S9(11) COMP-3.                   
NEWUNI    03 VENCOS-BDUCCO        PIC S9(11) COMP-3.                   
NEWUNI    03 UTIOPE-BDUCCO        PIC S9(11) COMP-3.                   
NEWUNI    03 UTINET-BDUCCO        PIC S9(11) COMP-3.
NEWUNI    03 FILLER               PIC X(02) .                   
