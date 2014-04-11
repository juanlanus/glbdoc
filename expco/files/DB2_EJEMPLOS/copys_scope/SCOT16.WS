REMARK*  SCOT16-WS.V.0019
REMARK**********************************************************************
REMARK* RMB0002        * ACIERT : VARAIBLE GENERALES DE ACIERTA
REMARK* RICARDO        * ACIE01 : VARAIBLE GENERALES DE ACIERTA SCORECARD 1*
REMARK* OCTUBRE.5.2002 * ACIE02 : VARAIBLE GENERALES DE ACIERTA SCORECARD 2*
REMARK*                * ACIE03 : VARAIBLE GENERALES DE ACIERTA SCORECARD 3*
REMARK*                * ACIE04 : VARAIBLE GENERALES DE ACIERTA SCORECARD 4*
REMARK*                * ACIE05 : VARAIBLE GENERALES DE ACIERTA SCORECARD 5*
REMARK**********************************************************************
REMARK* RMB0003     * AC2003 - SE CORRIGEN PROBLEMAS DE ACIERTA DE     *
REMARK* MARTINEZ    *        - ENDEUDAMIENTO GLOBAL,SCORE-CARD2 Y      *
REMARK* ENE.30.2003 *        - MOVIMIENTOS DE VARIABLES REDEFINIDAS    *
REMARK**********************************************************************
REMARK* RMB0004        * MICR01 : VARIABLES NECESARIAS PARA EL MANEJO DEL
REMARK* RICARDO MARTINE*          SCORE 6 PARA MICROCREDITO                *
REMARK* MARZ. 29 2005  * 
REMARK**********************************************************************
REMARK* RMB0005        * MICR02 : VARIABLES NECESARIAS PARA EL MANEJO DEL
REMARK* RICARDO MARTINE*          SCORE 6 PARA MICROCREDITO CON SCORE CARDS*
REMARK* JUNI. 15 2005  * 
REMARK**********************************************************************
REMARK* RMB0006        * FILT01 : SE APLICA EL FILTRO PARA ACIERTA Y ASIGNA*
REMARK* RICARDO MARTINE*          EN CASO DE CUMPLIRLO, SE MANEJAN LOS     *
REMARK* JUNI. 23 2005  *          CODIGOS DE RAZON 42 Y 72 (SCORE = 0)
REMARK**********************************************************************
REMARK* RMB0007        * MICR03 : VARIABLES NECESARIAS PARA EL MANEJO DEL  *
REMARK* RICARDO MARTINE*          SCORE BASICO VERSION III SIN ACIERTA     *
REMARK* SEPTIEMB.2005  *                                              
REMARK**********************************************************************
REMARK* DLM0008        * SMIN07 : CAMBIO EN LOS VALORES DE SALARIOS MINIMOS*
REMARK* DARIO LASSO M. *   QUE SE ENCUENTRAN EN LAS VARIABLES.             *
REMARK* 2007/01/12     *                                                   *
REMARK**********************************************************************
REMARK* RMB0009        * ACIEME - IMPLEMENTACION DEL SCORE ACIERTA-PLUS   
REMARK* MARTINEZ       *        - SE MODIFICAN LAS CARACTERISCAS ANTERIORES
REMARK* MAR.06.2007    *        - ADICIONANDO LAS NUEVAS PERO SE CONSERVA TODO
REMARK*                *        - DEBIDO A LOS MULTIPLES CAMBIOS SE ELIMINAN
REMARK*                *          TODOS LOS PATCH
REMARK**********************************************************************
REMARK* GJC0010        * SMIN08 - CAMBIO DEL SALARIO MINIMO PARA SER APLI-
REMARK* G. JIMENEZ     *          CADO EN 2008                              
REMARK* DICIEMBRE/2007 *                                                      
REMARK**********************************************************************
REMARK* RMB0011        * ACIEM9:  CAMBIO DEL USO DE CBAT120 POR CBAT120P *
REMARK* RICARDO MARTINE*          EN EL SCORE CARD 2                     *
REMARK* OCT-2008       * ACIC01:  SE INCLUYE VARIABLE TIQ006 REQUERIDA
REMARK*                *          PARA EL CALCULO DEL SCORE ACIERTA-C
REMARK**********************************************************************
REMARK* RMB0012        * DELTA0  SE ADICIONA MANEJO PARA DELTA SCORE ES   *
REMARK* RICARDO MARTINE*          DECIR CALCULO DE SCORE CON Y SIN AMNISTIA*
REMARK* OCT-2008       *          Y MOSTRAR O NO EL SCORE DE ACUERDO A LO- *
REMARK*                *          GICA DEFINIDA POR USA. INICIALMENTE OFF  *
REMARK**********************************************************************
REMARK* GJC0013        *  SMIN09 - CAMBIO DEL SALARIO MINIMO PARA SER APLI * 
REMARK* G.JIMENEZ      *          CADO EN 2009                             *
REMARK* ENERO 5/2009   *                                                   *
REMARK**********************************************************************
REMARK* GJC0014        *  ARTI13 - CREACION DE CAMPO PARA MANEJO O NO DE   * 
REMARK* G.JIMENEZ      *           LEY                                     *
REMARK* MAYO 6 /2009   *           CAMPO PARA MANEJAR ESTADO EN 24 MESES   *
REMARK**********************************************************************
REMARK* GJC0015        *  SMIN10 - CAMBIO DEL SALARIO MINIMO PARA SER APLI * 
REMARK* G.JIMENEZ      *          CADO EN 2010 - $515.000                  *
REMARK* DIC. 31/2009   *                                                   *
REMARK**********************************************************************
REMARK* CAV0016        *  SMIN11 - CAMBIO DEL SALARIO MINIMO PARA SER APLI * 
REMARK* C.VELASCO      *          CADO EN 2011 - $532.500                  *
REMARK* DIC. 31/2010   *                                                   *
REMARK**********************************************************************
REMARK* GJC0017        *  SAL112 - CAMBIO DEL SALARIO MINIMO PARA SER APLI * 
REMARK* G.JIMENEZ      *          CADO EN 2011 NUEVO DECRETO $535.600      * 
REMARK* ENE. 12/2011   *                                                   *
REMARK**********************************************************************
REMARK* GJC0018        *  SMIN12 - CAMBIO DEL SALARIO MINIMO PARA SER APLI * 
REMARK* G.JIMENEZ      *          CADO EN 2012 $566.700 DEJANDOLO COMO 567 * 
REMARK* DIC.28/2011    *                                                   *
REMARK**********************************************************************
REMARK* GJC0019        *  SMIN13 - CAMBIO DEL SALARIO MINIMO PARA SER APLI * 
REMARK* G.JIMENEZ      *          CADO EN 2013 $589.500 DEJANDOLO COMO 590 * 
REMARK* DIC.28/2012    *                                                   *
REMARK**********************************************************************
REMARK* CONJUNTO DE RUTINAS QUE PERMITE CALCULAR EL SCORE GENERICO DE RIESGO
REMARK* PREDICTA. ESTAS RUTINAS CALCULAN DE FORMA INTEGRA TODO EL SCORE,   *
REMARK* CALCULANDO PRIMERO TODAS LAS 340 CARACTERISTICAS INICIALES DEL MODE*
REMARK* LO, LUEGO TOMA LAS 40 QUE FUERON PREDICTIVAS Y CON ESTAS CALCULA EL*
REMARK* SCORE. SU FORMA DE LLAMADO ES:
REMARK* PERFORM 5000-TRU-SCORING-TOTAL                                     *
      *         PARAMETROS DE ENTRADA:        
REMARK*            TIP-ID-ALE  (TIPO DE IDENTIFICACION)
REMARK*            NUM-ID-ALE  (NUMERO DE IDENTIFICACION)
REMARK*            FECHA-RUN-TIME (FECHA DIA DE CORRIDA AAAAMM )
      *                 ( MOVE FUNCTION CURRENT-DATE(1:6) TO FECHA-RUN-D        
REMARK*            FECHA-TRI      (FECHA DIA DE CORRIDA AAAAMMDD )
      *                 (MOVE FUNCTION CURRENT-DATE(1:8) TO ULTIMO-TRI)        
REMARK*         VARIABLES DE SALIDA                                    
REMARK*            SCORE       (CALIFICACION PREDICTA)
REMARK*            ADVERSE-RAZON(1)  (CODIGO DE ACCION ADVERSA #1)
REMARK*            ADVERSE-RAZON(2)  (CODIGO DE ACCION ADVERSA #2)
REMARK*            ADVERSE-RAZON(3)  (CODIGO DE ACCION ADVERSA #3)
REMARK*            ADVERSE-RAZON(4)  (CODIGO DE ACCION ADVERSA #4)
REMARK*            SCORE-CARD        (NUMERO DEL SCORE CARD CLASIFICADO)
REMARK* (ESTRUCT)  REG-ALE O REG-CARACTERISTICAS (340 CARACTERSITICAS)
REMARK* (ESTRUCT)  VARIABLES-INTERMEDIAS         (VARIABLES DEL MODELO)
REMARK* (ESTRUCT)  VARIABLES-AA                  (VARIABLES EXCLUCION AAA)
REMARK* (ESTRUCT)  VARIABLES-ECUACION            (VARIABLES CALCULO SCORE)
REMARK* CORRE1  SE ELIMINAN LAS VARIABLES CRD13P,SUM054,OPOT24 Y CK00s POR *
REMARK*         VARIAR ANORMALMENTE EL SCORE (ABRIL 13/2000)               *
REMARK* CORRE2  SE MODIFICAN LOS AAC         (ABRIL 13/2000)
REMARK* CORRE3  SE QUITA LA PREGUNTA POR 999 (ABRIL 17/2000) POR CHICAGO   *
REMARK**********************************************************************
REMARK*         AQUI SE DEFINEN LAS ESTRUCTURAS Y VARIABLES UTILIZADAS EN  *
REMARK*         EL CALCULO DE CARACTERISTICAS Y SCORE                      *
REMARK* COMENT: EL LABEL "ACTUAL" TOCA ACTUALIZARLO CUANDO SE CAMBIE TIP-CTA
REMARK**********************************************************************

       01 REG-ALE-ACI-OUT.                                                      
          03 TAGS-ALE-ACI-OUT           PIC X(060).                             
          03 BLO-ID001-ID005-ACI-OUT    PIC X(012).                             
          03 BLO-CK001-CK002-ACI-OUT    PIC X(006).                             
          03 BLO-CR001-OT103-ACI-OUT    PIC X(816).                             
          03 BLO-GD001-GD050-ACI-OUT    PIC X(078).                             
          03 BLO-IQ001-IQ014-ACI-OUT    PIC X(027).                             
          03 BLO-IQ020-IQ023-ACI-OUT    PIC X(012).                             
          03 BLO-CR111-OT124-ACI-OUT    PIC X(360).                             
          03 BLO-AT112-AT124-ACI-OUT    PIC X(039).                             
          03 BLO-MT111-MT124-ACI-OUT    PIC X(090).                             
          03 COLITAS-ACI-OUT            PIC X(006).                             
       01 REG-ALE-PLU-OUT.                                                      
          03 TAGS-ALE-PLU-OUT           PIC X(012).                             
          03 BLO-ID001-ID005-PLU-OUT    PIC X(012).                             
          03 BLO-CK001-CK002-PLU-OUT    PIC X(006).                             
          03 BLO-CR001-OT103-PLU-OUT    PIC X(816).                             
          03 BLO-GD001-GD050-PLU-OUT    PIC X(078).                             
          03 BLO-IQ001-IQ014-PLU-OUT    PIC X(027).                             
          03 BLO-IQ020-IQ023-PLU-OUT    PIC X(012).                             
          03 BLO-CR111-OT124-PLU-OUT    PIC X(360).                             
          03 BLO-AT112-AT124-PLU-OUT    PIC X(039).                             
          03 BLO-MT111-MT124-PLU-OUT    PIC X(090).                             
          03 BLO-ID006-ID007-PLU-OUT    PIC X(005).                             
          03 BLO-CK003-CK006-PLU-OUT    PIC X(012).                             
          03 BLO-AH001-AH004-PLU-OUT    PIC X(012).                             
          03 BLO-IQ015-IQ015-PLU-OUT    PIC X(003).                             
          03 BLO-IQ024-IQ024-PLU-OUT    PIC X(003).                             
          03 BLO-IQ030-IQ032-PLU-OUT    PIC X(009).                             
          03 BLO-CR104-OT104-PLU-OUT    PIC X(12).                              
          03 BLO-CR105-OT105-PLU-OUT    PIC X(12).                              
          03 BLO-MT001-MT103-PLU-OUT    PIC X(204).                             
          03 BLO-MT104-MT104-PLU-OUT    PIC X(003).                             
          03 BLO-MT105-MT105-PLU-OUT    PIC X(003).                             
          03 BLO-CP001-CP103-PLU-OUT    PIC X(204).                             
          03 BLO-CP104-CP104-PLU-OUT    PIC X(003).                             
          03 BLO-CP105-CP105-PLU-OUT    PIC X(003).                             
          03 BLO-CP111-CP124-PLU-OUT    PIC X(090).                             
       01 REG-ALE                       PIC X(4000).                            
       01 REG-ALE-WORK.                                                         
          02 TAGS-ALE.        
             03 FILLER              PIC X(48).        
             03 NUM-ID-ALE          PIC 9(11).        
             03 TIP-ID-ALE          PIC 9.        
          02 BLO-ID001-ID005.        
             03 ID001                  PIC 9(3).        
             03 ID002                  PIC 9(2).        
             03 ID003                  PIC 9(2).        
             03 ID004                  PIC 9(3).        
             03 ID005                  PIC 9(2).        
          02 BLO-ID006-ID007.        
             03 ID006                  PIC 9(3).        
             03 ID007                  PIC 9(2).        
          02 BLO-CK001-CK002.            
             03 CK001                  PIC 9(3).        
             03 CK002                  PIC 9(3).        
          02 CAR-CARTERA.        
             03 BLO-CR001-OT103.        
                05 CR001              PIC X(003).        
                05 CR002              PIC X(003).        
                05 CR003              PIC X(003).        
                05 CR010              PIC X(003).        
                05 CR011              PIC X(003).        
                05 CR012              PIC X(003).        
                05 CR013              PIC X(003).        
                05 CR014              PIC X(003).        
                05 CR020              PIC X(003).        
                05 CR021              PIC X(003).        
                05 CR022              PIC X(003).        
                05 CR030              PIC X(003).        
                05 CR031              PIC X(003).        
                05 CR032              PIC X(003).        
                05 CR033              PIC X(003).        
                05 CR034              PIC X(003).        
                05 CR040              PIC X(003).        
                05 CR041              PIC X(003).        
                05 CR050              PIC X(003).        
                05 CR051              PIC X(003).        
                05 CR052              PIC X(003).        
                05 CR053              PIC X(003).        
                05 CR054              PIC X(003).        
                05 CR055              PIC X(003).        
                05 CR056              PIC X(003).        
                05 CR057              PIC X(003).        
                05 CR058              PIC X(003).        
                05 CR059              PIC X(003).        
                05 CR060              PIC X(003).        
                05 CR061              PIC X(003).        
                05 CR062              PIC X(003).        
                05 CR063              PIC X(003).        
                05 CR064              PIC X(003).        
                05 CR065              PIC X(003).        
                05 CR066              PIC X(003).        
                05 CR067              PIC X(003).        
                05 CR068              PIC X(003).        
                05 CR069              PIC X(003).        
                05 CR070              PIC X(003).        
                05 CR071              PIC X(003).        
                05 CR072              PIC X(003).        
                05 CR073              PIC X(003).        
                05 CR074              PIC X(003).        
                05 CR075              PIC X(003).        
                05 CR076              PIC X(003).        
                05 CR077              PIC X(003).        
                05 CR078              PIC X(003).        
                05 CR079              PIC X(003).        
                05 CR080              PIC X(003).        
                05 CR081              PIC X(003).        
                05 CR082              PIC X(003).        
                05 CR083              PIC X(003).        
                05 CR084              PIC X(003).        
                05 CR085              PIC X(003).        
                05 CR086              PIC X(003).        
                05 CR087              PIC X(003).        
                05 CR088              PIC X(003).        
                05 CR089              PIC X(003).        
                05 CR090              PIC X(003).        
                05 CR091              PIC X(003).        
                05 CR092              PIC X(003).        
                05 CR093              PIC X(003).        
                05 CR094              PIC X(003).        
                05 CR095              PIC X(003).        
                05 CR100              PIC X(003).        
                05 CR101              PIC X(003).        
                05 CR102              PIC X(003).        
                05 CR103              PIC X(003).        
                05 BS001              PIC X(003).        
                05 BS002              PIC X(003).        
                05 BS003              PIC X(003).        
                05 BS010              PIC X(003).        
                05 BS011              PIC X(003).        
                05 BS012              PIC X(003).        
                05 BS013              PIC X(003).        
                05 BS014              PIC X(003).        
                05 BS020              PIC X(003).        
                05 BS021              PIC X(003).        
                05 BS022              PIC X(003).        
                05 BS030              PIC X(003).        
                05 BS031              PIC X(003).        
                05 BS032              PIC X(003).        
                05 BS033              PIC X(003).        
                05 BS034              PIC X(003).        
                05 BS040              PIC X(003).        
                05 BS041              PIC X(003).        
                05 BS050              PIC X(003).        
                05 BS051              PIC X(003).        
                05 BS052              PIC X(003).        
                05 BS053              PIC X(003).        
                05 BS054              PIC X(003).        
                05 BS055              PIC X(003).        
                05 BS056              PIC X(003).        
                05 BS057              PIC X(003).        
                05 BS058              PIC X(003).        
                05 BS059              PIC X(003).        
                05 BS060              PIC X(003).        
                05 BS061              PIC X(003).        
                05 BS062              PIC X(003).        
                05 BS063              PIC X(003).        
                05 BS064              PIC X(003).        
                05 BS065              PIC X(003).        
                05 BS066              PIC X(003).        
                05 BS067              PIC X(003).        
                05 BS068              PIC X(003).        
                05 BS069              PIC X(003).        
                05 BS070              PIC X(003).        
                05 BS071              PIC X(003).        
                05 BS072              PIC X(003).        
                05 BS073              PIC X(003).        
                05 BS074              PIC X(003).        
                05 BS075              PIC X(003).        
                05 BS076              PIC X(003).        
                05 BS077              PIC X(003).        
                05 BS078              PIC X(003).        
                05 BS079              PIC X(003).        
                05 BS080              PIC X(003).        
                05 BS081              PIC X(003).        
                05 BS082              PIC X(003).        
                05 BS083              PIC X(003).        
                05 BS084              PIC X(003).        
                05 BS085              PIC X(003).        
                05 BS086              PIC X(003).        
                05 BS087              PIC X(003).        
                05 BS088              PIC X(003).        
                05 BS089              PIC X(003).        
                05 BS090              PIC X(003).        
                05 BS091              PIC X(003).        
                05 BS092              PIC X(003).        
                05 BS093              PIC X(003).        
                05 BS094              PIC X(003).        
                05 BS095              PIC X(003).        
                05 BS100              PIC X(003).        
                05 BS101              PIC X(003).        
                05 BS102              PIC X(003).        
                05 BS103              PIC X(003).        
                05 CC001              PIC X(003).        
                05 CC002              PIC X(003).        
                05 CC003              PIC X(003).        
                05 CC010              PIC X(003).        
                05 CC011              PIC X(003).        
                05 CC012              PIC X(003).        
                05 CC013              PIC X(003).        
                05 CC014              PIC X(003).        
                05 CC020              PIC X(003).        
                05 CC021              PIC X(003).        
                05 CC022              PIC X(003).        
                05 CC030              PIC X(003).        
                05 CC031              PIC X(003).        
                05 CC032              PIC X(003).        
                05 CC033              PIC X(003).        
                05 CC034              PIC X(003).        
                05 CC040              PIC X(003).        
                05 CC041              PIC X(003).        
                05 CC050              PIC X(003).        
                05 CC051              PIC X(003).        
                05 CC052              PIC X(003).        
                05 CC053              PIC X(003).        
                05 CC054              PIC X(003).        
                05 CC055              PIC X(003).        
                05 CC056              PIC X(003).        
                05 CC057              PIC X(003).        
                05 CC058              PIC X(003).        
                05 CC059              PIC X(003).        
                05 CC060              PIC X(003).        
                05 CC061              PIC X(003).        
                05 CC062              PIC X(003).        
                05 CC063              PIC X(003).        
                05 CC064              PIC X(003).        
                05 CC065              PIC X(003).        
                05 CC066              PIC X(003).        
                05 CC067              PIC X(003).        
                05 CC068              PIC X(003).        
                05 CC069              PIC X(003).        
                05 CC070              PIC X(003).        
                05 CC071              PIC X(003).        
                05 CC072              PIC X(003).        
                05 CC073              PIC X(003).        
                05 CC074              PIC X(003).        
                05 CC075              PIC X(003).        
                05 CC076              PIC X(003).        
                05 CC077              PIC X(003).        
                05 CC078              PIC X(003).        
                05 CC079              PIC X(003).        
                05 CC080              PIC X(003).        
                05 CC081              PIC X(003).        
                05 CC082              PIC X(003).        
                05 CC083              PIC X(003).        
                05 CC084              PIC X(003).        
                05 CC085              PIC X(003).        
                05 CC086              PIC X(003).        
                05 CC087              PIC X(003).        
                05 CC088              PIC X(003).        
                05 CC089              PIC X(003).        
                05 CC090              PIC X(003).        
                05 CC091              PIC X(003).        
                05 CC092              PIC X(003).        
                05 CC093              PIC X(003).        
                05 CC094              PIC X(003).        
                05 CC095              PIC X(003).        
                05 CC100              PIC X(003).        
                05 CC101              PIC X(003).        
                05 CC102              PIC X(003).        
                05 CC103              PIC X(003).        
                05 OT001              PIC X(003).        
                05 OT002              PIC X(003).        
                05 OT003              PIC X(003).        
                05 OT010              PIC X(003).        
                05 OT011              PIC X(003).        
                05 OT012              PIC X(003).        
                05 OT013              PIC X(003).        
                05 OT014              PIC X(003).        
                05 OT020              PIC X(003).        
                05 OT021              PIC X(003).        
                05 OT022              PIC X(003).        
                05 OT030              PIC X(003).        
                05 OT031              PIC X(003).        
                05 OT032              PIC X(003).        
                05 OT033              PIC X(003).        
                05 OT034              PIC X(003).        
                05 OT040              PIC X(003).        
                05 OT041              PIC X(003).        
                05 OT050              PIC X(003).        
                05 OT051              PIC X(003).        
                05 OT052              PIC X(003).        
                05 OT053              PIC X(003).        
                05 OT054              PIC X(003).        
                05 OT055              PIC X(003).        
                05 OT056              PIC X(003).        
                05 OT057              PIC X(003).        
                05 OT058              PIC X(003).        
                05 OT059              PIC X(003).        
                05 OT060              PIC X(003).        
                05 OT061              PIC X(003).        
                05 OT062              PIC X(003).        
                05 OT063              PIC X(003).        
                05 OT064              PIC X(003).        
                05 OT065              PIC X(003).        
                05 OT066              PIC X(003).        
                05 OT067              PIC X(003).        
                05 OT068              PIC X(003).        
                05 OT069              PIC X(003).        
                05 OT070              PIC X(003).        
                05 OT071              PIC X(003).        
                05 OT072              PIC X(003).        
                05 OT073              PIC X(003).        
                05 OT074              PIC X(003).        
                05 OT075              PIC X(003).        
                05 OT076              PIC X(003).        
                05 OT077              PIC X(003).        
                05 OT078              PIC X(003).        
                05 OT079              PIC X(003).        
                05 OT080              PIC X(003).        
                05 OT081              PIC X(003).        
                05 OT082              PIC X(003).        
                05 OT083              PIC X(003).        
                05 OT084              PIC X(003).        
                05 OT085              PIC X(003).        
                05 OT086              PIC X(003).        
                05 OT087              PIC X(003).        
                05 OT088              PIC X(003).        
                05 OT089              PIC X(003).        
                05 OT090              PIC X(003).        
                05 OT091              PIC X(003).        
                05 OT092              PIC X(003).        
                05 OT093              PIC X(003).        
                05 OT094              PIC X(003).        
                05 OT095              PIC X(003).        
                05 OT100              PIC X(003).        
                05 OT101              PIC X(003).        
                05 OT102              PIC X(003).        
                05 OT103              PIC X(003).        
             03 BLO-MT001-MT103.        
                05 MT001              PIC X(003).        
                05 MT002              PIC X(003).        
                05 MT003              PIC X(003).        
                05 MT010              PIC X(003).        
                05 MT011              PIC X(003).        
                05 MT012              PIC X(003).        
                05 MT013              PIC X(003).        
                05 MT014              PIC X(003).        
                05 MT020              PIC X(003).        
                05 MT021              PIC X(003).        
                05 MT022              PIC X(003).        
                05 MT030              PIC X(003).        
                05 MT031              PIC X(003).        
                05 MT032              PIC X(003).        
                05 MT033              PIC X(003).        
                05 MT034              PIC X(003).        
                05 MT040              PIC X(003).        
                05 MT041              PIC X(003).        
                05 MT050              PIC X(003).        
                05 MT051              PIC X(003).        
                05 MT052              PIC X(003).        
                05 MT053              PIC X(003).        
                05 MT054              PIC X(003).        
                05 MT055              PIC X(003).        
                05 MT056              PIC X(003).        
                05 MT057              PIC X(003).        
                05 MT058              PIC X(003).        
                05 MT059              PIC X(003).        
                05 MT060              PIC X(003).        
                05 MT061              PIC X(003).        
                05 MT062              PIC X(003).        
                05 MT063              PIC X(003).        
                05 MT064              PIC X(003).        
                05 MT065              PIC X(003).        
                05 MT066              PIC X(003).        
                05 MT067              PIC X(003).        
                05 MT068              PIC X(003).        
                05 MT069              PIC X(003).        
                05 MT070              PIC X(003).        
                05 MT071              PIC X(003).        
                05 MT072              PIC X(003).        
                05 MT073              PIC X(003).        
                05 MT074              PIC X(003).        
                05 MT075              PIC X(003).        
                05 MT076              PIC X(003).        
                05 MT077              PIC X(003).        
                05 MT078              PIC X(003).        
                05 MT079              PIC X(003).        
                05 MT080              PIC X(003).        
                05 MT081              PIC X(003).        
                05 MT082              PIC X(003).        
                05 MT083              PIC X(003).        
                05 MT084              PIC X(003).        
                05 MT085              PIC X(003).        
                05 MT086              PIC X(003).        
                05 MT087              PIC X(003).        
                05 MT088              PIC X(003).        
                05 MT089              PIC X(003).        
                05 MT090              PIC X(003).        
                05 MT091              PIC X(003).        
                05 MT092              PIC X(003).        
                05 MT093              PIC X(003).        
                05 MT094              PIC X(003).        
                05 MT095              PIC X(003).        
                05 MT100              PIC X(003).        
                05 MT101              PIC X(003).        
                05 MT102              PIC X(003).        
                05 MT103              PIC X(003).        
             03 BLO-CP001-CP103.        
                05 CP001              PIC X(003).        
                05 CP002              PIC X(003).        
                05 CP003              PIC X(003).        
                05 CP010              PIC X(003).        
                05 CP011              PIC X(003).        
                05 CP012              PIC X(003).        
                05 CP013              PIC X(003).        
                05 CP014              PIC X(003).        
                05 CP020              PIC X(003).        
                05 CP021              PIC X(003).        
                05 CP022              PIC X(003).        
                05 CP030              PIC X(003).        
                05 CP031              PIC X(003).        
                05 CP032              PIC X(003).        
                05 CP033              PIC X(003).        
                05 CP034              PIC X(003).        
                05 CP040              PIC X(003).        
                05 CP041              PIC X(003).        
                05 CP050              PIC X(003).        
                05 CP051              PIC X(003).        
                05 CP052              PIC X(003).        
                05 CP053              PIC X(003).        
                05 CP054              PIC X(003).        
                05 CP055              PIC X(003).        
                05 CP056              PIC X(003).        
                05 CP057              PIC X(003).        
                05 CP058              PIC X(003).        
                05 CP059              PIC X(003).        
                05 CP060              PIC X(003).        
                05 CP061              PIC X(003).        
                05 CP062              PIC X(003).        
                05 CP063              PIC X(003).        
                05 CP064              PIC X(003).        
                05 CP065              PIC X(003).        
                05 CP066              PIC X(003).        
                05 CP067              PIC X(003).        
                05 CP068              PIC X(003).        
                05 CP069              PIC X(003).        
                05 CP070              PIC X(003).        
                05 CP071              PIC X(003).        
                05 CP072              PIC X(003).        
                05 CP073              PIC X(003).        
                05 CP074              PIC X(003).        
                05 CP075              PIC X(003).        
                05 CP076              PIC X(003).        
                05 CP077              PIC X(003).        
                05 CP078              PIC X(003).        
                05 CP079              PIC X(003).        
                05 CP080              PIC X(003).        
                05 CP081              PIC X(003).        
                05 CP082              PIC X(003).        
                05 CP083              PIC X(003).        
                05 CP084              PIC X(003).        
                05 CP085              PIC X(003).        
                05 CP086              PIC X(003).        
                05 CP087              PIC X(003).        
                05 CP088              PIC X(003).        
                05 CP089              PIC X(003).        
                05 CP090              PIC X(003).        
                05 CP091              PIC X(003).        
                05 CP092              PIC X(003).        
                05 CP093              PIC X(003).        
                05 CP094              PIC X(003).        
                05 CP095              PIC X(003).        
                05 CP100              PIC X(003).        
                05 CP101              PIC X(003).        
                05 CP102              PIC X(003).        
                05 CP103              PIC X(003).        
          02 FILLER REDEFINES CAR-CARTERA.        
             03 V-TIPO-ALE OCCURS 6 TIMES.                                      
                04 V-CARTERA OCCURS  68 TIMES PIC X(3).        
          02 FILLER REDEFINES CAR-CARTERA.        
             03 V-CARTERAS OCCURS 408 TIMES PIC X(003).                         
          02 BLO-GD001-GD050.              
             03 GD00X        OCCURS   3 TIMES PIC 9(3).        
             03 GD01X        OCCURS   3 TIMES PIC 9(7).        
             03 FILLER       OCCURS   3 TIMES.        
                04 GDXXX     OCCURS   5 TIMES PIC 9(3).        
             03 GD050                  PIC 9(3).        
          02 FILLER REDEFINES BLO-GD001-GD050.        
             03 GD001              PIC X(003).        
             03 GD002              PIC X(003).        
             03 GD003              PIC X(003).        
             03 GD010              PIC X(007).        
             03 GD011              PIC X(007).        
             03 GD012              PIC X(007).        
             03 GD020              PIC X(003).        
             03 GD021              PIC X(003).        
             03 GD022              PIC X(003).        
             03 GD023              PIC X(003).        
             03 GD024              PIC X(003).        
             03 GD030              PIC X(003).        
             03 GD031              PIC X(003).        
             03 GD032              PIC X(003).        
             03 GD033              PIC X(003).        
             03 GD034              PIC X(003).        
             03 GD040              PIC X(003).        
             03 GD041              PIC X(003).        
             03 GD042              PIC X(003).        
             03 GD043              PIC X(003).        
             03 GD044              PIC X(003).        
             03 FILLER-GD050       PIC X(003).        
          02 BLO-IQ001-IQ024.        
             03 IQ00X        OCCURS   5 TIMES PIC 9(3).        
             03 IQ01X        OCCURS   5 TIMES PIC 9(3).                         
             03 IQ02X        OCCURS   5 TIMES PIC 9(3).                         
          02 FILLER REDEFINES BLO-IQ001-IQ024.        
             03 BLO-IQ001-IQ014.                                                
                04 IQ001              PIC X(003).        
                04 IQ002              PIC X(003).        
                04 IQ003              PIC X(003).        
                04 IQ004              PIC X(003).        
                04 IQ005              PIC X(003).        
                04 IQ011              PIC X(003).        
                04 IQ012              PIC X(003).        
                04 IQ013              PIC X(003).        
                04 IQ014              PIC X(003).        
             03 BLO-IQ015-IQ015.                                                
                04 IQ015              PIC X(003).                               
             03 BLO-IQ020-IQ023.                                                
                04 IQ020              PIC X(003).        
                04 IQ021              PIC X(003).        
                04 IQ022              PIC X(003).        
                04 IQ023              PIC X(003).        
             03 BLO-IQ024-IQ024.                                                
                04 IQ024              PIC X(003).                               
          02 GRU-CR111-CP124.                                                   
             03 V-TIPO-BAL OCCURS 6 TIMES.                                      
                04 BAL111                     PIC 9(9).              
                04 BAL112                     PIC 9(9).             
                04 BAL113                     PIC 9(9).            
                04 BAL114                     PIC 9(9).           
                04 BAL115                     PIC 9(4)V99.             
                04 BAL116                     PIC 9(4)V99.            
                04 BAL117                     PIC 9(3).              
                04 BAL118                     PIC 9(3).             
                04 BAL119                     PIC 9(3).            
                04 BAL120                     PIC 9(3).           
                04 BAL121                     PIC 9(3).          
                04 BAL122                     PIC 9(9).                    
                04 BAL123                     PIC 9(9).                   
                04 BAL124                     PIC 9(9).                  
          02  FILLER REDEFINES GRU-CR111-CP124.        
             03 BLO-CR111-OT124.                         
                04 CR111              PIC X(009).           
                04 CR112              PIC X(009).          
                04 CR113              PIC X(009).         
                04 CR114              PIC X(009).        
                04 CR115              PIC X(006).        
                04 CR116              PIC X(006).        
                04 CR117              PIC X(003).        
                04 CR118              PIC X(003).        
                04 CR119              PIC X(003).        
                04 CR120              PIC X(003).        
                04 CR121              PIC X(003).        
                04 CR122              PIC X(009).        
                04 CR123              PIC X(009).        
                04 CR124              PIC X(009).        
                04 BS111              PIC X(009).        
                04 BS112              PIC X(009).        
                04 BS113              PIC X(009).        
                04 BS114              PIC X(009).        
                04 BS115              PIC X(006).        
                04 BS116              PIC X(006).        
                04 BS117              PIC X(003).        
                04 BS118              PIC X(003).        
                04 BS119              PIC X(003).        
                04 BS120              PIC X(003).        
                04 BS121              PIC X(003).        
                04 BS122              PIC X(009).        
                04 BS123              PIC X(009).        
                04 BS124              PIC X(009).        
                04 CC111              PIC X(009).        
                04 CC112              PIC X(009).        
                04 CC113              PIC X(009).        
                04 CC114              PIC X(009).        
                04 CC115              PIC X(006).        
                04 CC116              PIC X(006).        
                04 CC117              PIC X(003).        
                04 CC118              PIC X(003).        
                04 CC119              PIC X(003).        
                04 CC120              PIC X(003).        
                04 CC121              PIC X(003).        
                04 CC122              PIC X(009).        
                04 CC123              PIC X(009).        
                04 CC124              PIC X(009).        
                04 OT111              PIC X(009).        
                04 OT112              PIC X(009).        
                04 OT113              PIC X(009).        
                04 OT114              PIC X(009).        
                04 OT115              PIC X(006).        
                04 OT116              PIC X(006).        
                04 OT117              PIC X(003).        
                04 OT118              PIC X(003).        
                04 OT119              PIC X(003).        
                04 OT120              PIC X(003).        
                04 OT121              PIC X(003).        
                04 OT122              PIC X(009).        
                04 OT123              PIC X(009).        
                04 OT124              PIC X(009).        
             03 BLO-MT111-MT124.                         
                04 HIP111                     PIC 9(9).              
                04 HIP112                     PIC 9(9).             
                04 HIP113                     PIC 9(9).                
                04 HIP114                     PIC 9(9).                
                04 HIP115                     PIC 9(4)V99.             
                04 HIP116                     PIC 9(4)V99.             
                04 HIP117                     PIC 9(3).                
                04 HIP118                     PIC 9(3).                
                04 HIP119                     PIC 9(3).                
                04 HIP120                     PIC 9(3).                
                04 HIP121                     PIC 9(3).                
                04 HIP122                     PIC 9(9).                
                04 HIP123                     PIC 9(9).                
                04 HIP124                     PIC 9(9).                
             03 FILLER REDEFINES BLO-MT111-MT124.                      
                04 MT111              PIC X(009).                      
                04 MT112              PIC X(009).                      
                04 MT113              PIC X(009).                      
                04 MT114              PIC X(009).                     
                04 MT115              PIC X(006).                    
                04 MT116              PIC X(006).                   
                04 MT117              PIC X(003).                  
                04 MT118              PIC X(003).                 
                04 MT119              PIC X(003).                
                04 MT120              PIC X(003).               
                04 MT121              PIC X(003).              
                04 MT122              PIC X(009).             
                04 MT123              PIC X(009).            
                04 MT124              PIC X(009).           
             03 BLO-CP111-CP124.                         
                04 COD111                     PIC 9(9).           
                04 COD112                     PIC 9(9).          
                04 COD113                     PIC 9(9).             
                04 COD114                     PIC 9(9).            
                04 COD115                     PIC 9(4)V99.          
                04 COD116                     PIC 9(4)V99.          
                04 COD117                     PIC 9(3).             
                04 COD118                     PIC 9(3).             
                04 COD119                     PIC 9(3).             
                04 COD120                     PIC 9(3).             
                04 COD121                     PIC 9(3).             
                04 COD122                     PIC 9(9).             
                04 COD123                     PIC 9(9).             
                04 COD124                     PIC 9(9).             
             03 FILLER REDEFINES BLO-CP111-CP124.                   
                04 CP111              PIC X(009).                  
                04 CP112              PIC X(009).                 
                04 CP113              PIC X(009).                
                04 CP114              PIC X(009).               
                04 CP115              PIC X(006).              
                04 CP116              PIC X(006).             
                04 CP117              PIC X(003).            
                04 CP118              PIC X(003).           
                04 CP119              PIC X(003).          
                04 CP120              PIC X(003).           
                04 CP121              PIC X(003).           
                04 CP122              PIC X(009).           
                04 CP123              PIC X(009).           
                04 CP124              PIC X(009).           
          02 BLO-AT112-AT124.                                                   
             03 PRO112                     PIC 9(9).                            
             03 PRO114                     PIC 9(9).                            
             03 PRO115                     PIC 9(4)V99.                         
             03 PRO116                     PIC 9(4)V99.                         
             03 PRO124                     PIC 9(9).                            
          02 FILLER REDEFINES BLO-AT112-AT124.                                  
             03 AT112              PIC X(009).                                  
             03 AT114              PIC X(009).                                  
             03 AT115              PIC X(006).                                  
             03 AT116              PIC X(006).                                  
             03 AT124              PIC X(009).                                  
          02 BLO-CK003-CK006.        
             03 CK003                  PIC 9(3).        
             03 CK004                  PIC 9(3).        
             03 CK005                  PIC 9(3).        
             03 CK006                  PIC 9(3).        
          02 BLO-AH001-AH004.        
             03 AH001                  PIC 9(3).        
             03 AH002                  PIC 9(3).        
             03 AH003                  PIC 9(3).        
             03 AH004                  PIC 9(3).        
          02 BLO-IQ030-IQ032.        
             03 IQ030                  PIC 9(3).        
             03 IQ031                  PIC 9(3).        
             03 IQ032                  PIC 9(3).        
          02 BLOQUE-104.                                                        
             03 V-104 OCCURS 6 TIMES      PIC 9(03).                            
          02 FILLER REDEFINES BLOQUE-104.                                       
             03 BLO-CR104-OT104.              
                04 CR104                  PIC X(003).        
                04 BS104                  PIC X(003).        
                04 CC104                  PIC X(003).        
                04 OT104                  PIC X(003).        
             03 BLO-MT104-MT104.        
                04 MT104                  PIC X(003).        
             03 BLO-CP104-CP104.        
                04 CP104                  PIC X(003).        
          02 BLOQUE-105.                                                        
             03 V-105 OCCURS 6 TIMES      PIC 9(03).                            
          02 FILLER REDEFINES BLOQUE-105.                                       
             03 BLO-CR105-OT105.                                                
                04 CR105                  PIC X(003).                           
                04 BS105                  PIC X(003).                           
                04 CC105                  PIC X(003).                           
                04 OT105                  PIC X(003).                           
             03 BLO-MT105-MT105.                                                
                04 MT105                  PIC X(003).                           
             03 BLO-CP105-CP105.                                                
                04 CP105                  PIC X(003).                           
          02 COLITAS.        
             03 MASTAGS-ALE                PIC X(3).        
             03 SCORE-ALE-1                PIC 9(03).          
          02 FILLER REDEFINES COLITAS.        
             03 FILLER-OTROS               PIC X(003).        
             03 SCORE-ALE                  PIC 9(3).        
          02 M-DISPUTAS.                                                        
             03 V-DISPUTAS OCCURS 6 TIMES    PIC 9(03).                         
      *-----------------------------------------------------------------        
       01 REG-INP.                         
          02 PARTE1-REG-INP          PIC X(60).        
          02 PARTE2-REG-INP          PIC X(3).        
       01 TREG-CARACTERISTICAS.        
          02 TCK001                PIC 9(003).        
          02 TCK002                PIC 9(003).                                  
          02 TCAR-CARTERA.        
             03 TCR001              PIC 9(003).        
             03 TCR002              PIC 9(003).        
             03 TCR003              PIC 9(003).        
             03 TCR010              PIC 9(003).        
             03 TCR011              PIC 9(003).        
             03 TCR012              PIC 9(003).        
             03 TCR013              PIC 9(003).        
             03 TCR014              PIC 9(003).        
             03 TCR020              PIC 9(003).        
             03 TCR021              PIC 9(003).        
             03 TCR022              PIC 9(003).        
             03 TCR030              PIC 9(003).        
             03 TCR031              PIC 9(003).        
             03 TCR032              PIC 9(003).        
             03 TCR033              PIC 9(003).        
             03 TCR034              PIC 9(003).        
             03 TCR040              PIC 9(003).        
             03 TCR041              PIC 9(003).        
             03 TCR050              PIC 9(003).        
             03 TCR051              PIC 9(003).        
             03 TCR052              PIC 9(003).        
             03 TCR053              PIC 9(003).        
             03 TCR054              PIC 9(003).        
             03 TCR055              PIC 9(003).        
             03 TCR056              PIC 9(003).        
             03 TCR057              PIC 9(003).        
             03 TCR058              PIC 9(003).        
             03 TCR059              PIC 9(003).        
             03 TCR060              PIC 9(003).        
             03 TCR061              PIC 9(003).        
             03 TCR062              PIC 9(003).        
             03 TCR063              PIC 9(003).        
             03 TCR064              PIC 9(003).        
             03 TCR065              PIC 9(003).        
             03 TCR066              PIC 9(003).        
             03 TCR067              PIC 9(003).        
             03 TCR068              PIC 9(003).        
             03 TCR069              PIC 9(003).        
             03 TCR070              PIC 9(003).        
             03 TCR071              PIC 9(003).        
             03 TCR072              PIC 9(003).        
             03 TCR073              PIC 9(003).        
             03 TCR074              PIC 9(003).        
             03 TCR075              PIC 9(003).        
             03 TCR076              PIC 9(003).        
             03 TCR077              PIC 9(003).        
             03 TCR078              PIC 9(003).        
             03 TCR079              PIC 9(003).        
             03 TCR080              PIC 9(003).        
             03 TCR081              PIC 9(003).        
             03 TCR082              PIC 9(003).        
             03 TCR083              PIC 9(003).        
             03 TCR084              PIC 9(003).        
             03 TCR085              PIC 9(003).        
             03 TCR086              PIC 9(003).        
             03 TCR087              PIC 9(003).        
             03 TCR088              PIC 9(003).        
             03 TCR089              PIC 9(003).        
             03 TCR090              PIC 9(003).        
             03 TCR091              PIC 9(003).        
             03 TCR092              PIC 9(003).        
             03 TCR093              PIC 9(003).        
             03 TCR094              PIC 9(003).        
             03 TCR095              PIC 9(003).        
             03 TCR100              PIC 9(003).        
             03 TCR101              PIC 9(003).        
             03 TCR102              PIC 9(003).        
             03 TCR103              PIC 9(003).        
             03 TBS001              PIC 9(003).        
             03 TBS002              PIC 9(003).        
             03 TBS003              PIC 9(003).        
             03 TBS010              PIC 9(003).        
             03 TBS011              PIC 9(003).        
             03 TBS012              PIC 9(003).        
             03 TBS013              PIC 9(003).        
             03 TBS014              PIC 9(003).        
             03 TBS020              PIC 9(003).        
             03 TBS021              PIC 9(003).        
             03 TBS022              PIC 9(003).        
             03 TBS030              PIC 9(003).        
             03 TBS031              PIC 9(003).        
             03 TBS032              PIC 9(003).        
             03 TBS033              PIC 9(003).        
             03 TBS034              PIC 9(003).        
             03 TBS040              PIC 9(003).        
             03 TBS041              PIC 9(003).        
             03 TBS050              PIC 9(003).        
             03 TBS051              PIC 9(003).        
             03 TBS052              PIC 9(003).        
             03 TBS053              PIC 9(003).        
             03 TBS054              PIC 9(003).        
             03 TBS055              PIC 9(003).        
             03 TBS056              PIC 9(003).        
             03 TBS057              PIC 9(003).        
             03 TBS058              PIC 9(003).        
             03 TBS059              PIC 9(003).        
             03 TBS060              PIC 9(003).        
             03 TBS061              PIC 9(003).        
             03 TBS062              PIC 9(003).        
             03 TBS063              PIC 9(003).        
             03 TBS064              PIC 9(003).        
             03 TBS065              PIC 9(003).        
             03 TBS066              PIC 9(003).        
             03 TBS067              PIC 9(003).        
             03 TBS068              PIC 9(003).        
             03 TBS069              PIC 9(003).        
             03 TBS070              PIC 9(003).        
             03 TBS071              PIC 9(003).        
             03 TBS072              PIC 9(003).        
             03 TBS073              PIC 9(003).        
             03 TBS074              PIC 9(003).        
             03 TBS075              PIC 9(003).        
             03 TBS076              PIC 9(003).        
             03 TBS077              PIC 9(003).        
             03 TBS078              PIC 9(003).        
             03 TBS079              PIC 9(003).        
             03 TBS080              PIC 9(003).        
             03 TBS081              PIC 9(003).        
             03 TBS082              PIC 9(003).        
             03 TBS083              PIC 9(003).        
             03 TBS084              PIC 9(003).        
             03 TBS085              PIC 9(003).        
             03 TBS086              PIC 9(003).        
             03 TBS087              PIC 9(003).        
             03 TBS088              PIC 9(003).        
             03 TBS089              PIC 9(003).        
             03 TBS090              PIC 9(003).        
             03 TBS091              PIC 9(003).        
             03 TBS092              PIC 9(003).        
             03 TBS093              PIC 9(003).        
             03 TBS094              PIC 9(003).        
             03 TBS095              PIC 9(003).        
             03 TBS100              PIC 9(003).        
             03 TBS101              PIC 9(003).        
             03 TBS102              PIC 9(003).        
             03 TBS103              PIC 9(003).        
             03 TCC001              PIC 9(003).        
             03 TCC002              PIC 9(003).        
             03 TCC003              PIC 9(003).        
             03 TCC010              PIC 9(003).        
             03 TCC011              PIC 9(003).        
             03 TCC012              PIC 9(003).        
             03 TCC013              PIC 9(003).        
             03 TCC014              PIC 9(003).        
             03 TCC020              PIC 9(003).        
             03 TCC021              PIC 9(003).        
             03 TCC022              PIC 9(003).        
             03 TCC030              PIC 9(003).        
             03 TCC031              PIC 9(003).        
             03 TCC032              PIC 9(003).        
             03 TCC033              PIC 9(003).        
             03 TCC034              PIC 9(003).        
             03 TCC040              PIC 9(003).        
             03 TCC041              PIC 9(003).        
             03 TCC050              PIC 9(003).        
             03 TCC051              PIC 9(003).        
             03 TCC052              PIC 9(003).        
             03 TCC053              PIC 9(003).        
             03 TCC054              PIC 9(003).        
             03 TCC055              PIC 9(003).        
             03 TCC056              PIC 9(003).        
             03 TCC057              PIC 9(003).        
             03 TCC058              PIC 9(003).        
             03 TCC059              PIC 9(003).        
             03 TCC060              PIC 9(003).        
             03 TCC061              PIC 9(003).        
             03 TCC062              PIC 9(003).        
             03 TCC063              PIC 9(003).        
             03 TCC064              PIC 9(003).        
             03 TCC065              PIC 9(003).        
             03 TCC066              PIC 9(003).        
             03 TCC067              PIC 9(003).        
             03 TCC068              PIC 9(003).        
             03 TCC069              PIC 9(003).        
             03 TCC070              PIC 9(003).        
             03 TCC071              PIC 9(003).        
             03 TCC072              PIC 9(003).        
             03 TCC073              PIC 9(003).        
             03 TCC074              PIC 9(003).        
             03 TCC075              PIC 9(003).        
             03 TCC076              PIC 9(003).        
             03 TCC077              PIC 9(003).        
             03 TCC078              PIC 9(003).        
             03 TCC079              PIC 9(003).        
             03 TCC080              PIC 9(003).        
             03 TCC081              PIC 9(003).        
             03 TCC082              PIC 9(003).        
             03 TCC083              PIC 9(003).        
             03 TCC084              PIC 9(003).        
             03 TCC085              PIC 9(003).        
             03 TCC086              PIC 9(003).        
             03 TCC087              PIC 9(003).        
             03 TCC088              PIC 9(003).        
             03 TCC089              PIC 9(003).        
             03 TCC090              PIC 9(003).        
             03 TCC091              PIC 9(003).        
             03 TCC092              PIC 9(003).        
             03 TCC093              PIC 9(003).        
             03 TCC094              PIC 9(003).        
             03 TCC095              PIC 9(003).        
             03 TCC100              PIC 9(003).        
             03 TCC101              PIC 9(003).        
             03 TCC102              PIC 9(003).        
             03 TCC103              PIC 9(003).        
             03 TOT001              PIC 9(003).        
             03 TOT002              PIC 9(003).        
             03 TOT003              PIC 9(003).        
             03 TOT010              PIC 9(003).        
             03 TOT011              PIC 9(003).        
             03 TOT012              PIC 9(003).        
             03 TOT013              PIC 9(003).        
             03 TOT014              PIC 9(003).        
             03 TOT020              PIC 9(003).        
             03 TOT021              PIC 9(003).        
             03 TOT022              PIC 9(003).        
             03 TOT030              PIC 9(003).        
             03 TOT031              PIC 9(003).        
             03 TOT032              PIC 9(003).        
             03 TOT033              PIC 9(003).        
             03 TOT034              PIC 9(003).        
             03 TOT040              PIC 9(003).        
             03 TOT041              PIC 9(003).        
             03 TOT050              PIC 9(003).        
             03 TOT051              PIC 9(003).        
             03 TOT052              PIC 9(003).        
             03 TOT053              PIC 9(003).        
             03 TOT054              PIC 9(003).        
             03 TOT055              PIC 9(003).        
             03 TOT056              PIC 9(003).        
             03 TOT057              PIC 9(003).        
             03 TOT058              PIC 9(003).        
             03 TOT059              PIC 9(003).        
             03 TOT060              PIC 9(003).        
             03 TOT061              PIC 9(003).        
             03 TOT062              PIC 9(003).        
             03 TOT063              PIC 9(003).        
             03 TOT064              PIC 9(003).        
             03 TOT065              PIC 9(003).        
             03 TOT066              PIC 9(003).        
             03 TOT067              PIC 9(003).        
             03 TOT068              PIC 9(003).        
             03 TOT069              PIC 9(003).        
             03 TOT070              PIC 9(003).        
             03 TOT071              PIC 9(003).        
             03 TOT072              PIC 9(003).        
             03 TOT073              PIC 9(003).        
             03 TOT074              PIC 9(003).        
             03 TOT075              PIC 9(003).        
             03 TOT076              PIC 9(003).        
             03 TOT077              PIC 9(003).        
             03 TOT078              PIC 9(003).        
             03 TOT079              PIC 9(003).        
             03 TOT080              PIC 9(003).        
             03 TOT081              PIC 9(003).        
             03 TOT082              PIC 9(003).        
             03 TOT083              PIC 9(003).        
             03 TOT084              PIC 9(003).        
             03 TOT085              PIC 9(003).        
             03 TOT086              PIC 9(003).        
             03 TOT087              PIC 9(003).        
             03 TOT088              PIC 9(003).        
             03 TOT089              PIC 9(003).        
             03 TOT090              PIC 9(003).        
             03 TOT091              PIC 9(003).        
             03 TOT092              PIC 9(003).        
             03 TOT093              PIC 9(003).        
             03 TOT094              PIC 9(003).        
             03 TOT095              PIC 9(003).        
             03 TOT100              PIC 9(003).        
             03 TOT101              PIC 9(003).        
             03 TOT102              PIC 9(003).        
             03 TOT103              PIC 9(003).        
             03 TMT001              PIC 9(003).                                 
             03 TMT002              PIC 9(003).                                 
             03 TMT003              PIC 9(003).                                 
             03 TMT010              PIC 9(003).                                 
             03 TMT011              PIC 9(003).                                 
             03 TMT012              PIC 9(003).                                 
             03 TMT013              PIC 9(003).                                 
             03 TMT014              PIC 9(003).                                 
             03 TMT020              PIC 9(003).                                 
             03 TMT021              PIC 9(003).                                 
             03 TMT022              PIC 9(003).                                 
             03 TMT030              PIC 9(003).                                 
             03 TMT031              PIC 9(003).                                 
             03 TMT032              PIC 9(003).                                 
             03 TMT033              PIC 9(003).                                 
             03 TMT034              PIC 9(003).                                 
             03 TMT040              PIC 9(003).                                 
             03 TMT041              PIC 9(003).                                 
             03 TMT050              PIC 9(003).                                 
             03 TMT051              PIC 9(003).                                 
             03 TMT052              PIC 9(003).                                 
             03 TMT053              PIC 9(003).                                 
             03 TMT054              PIC 9(003).                                 
             03 TMT055              PIC 9(003).                                 
             03 TMT056              PIC 9(003).                                 
             03 TMT057              PIC 9(003).                                 
             03 TMT058              PIC 9(003).                                 
             03 TMT059              PIC 9(003).                                 
             03 TMT060              PIC 9(003).                                 
             03 TMT061              PIC 9(003).                                 
             03 TMT062              PIC 9(003).                                 
             03 TMT063              PIC 9(003).                                 
             03 TMT064              PIC 9(003).                                 
             03 TMT065              PIC 9(003).                                 
             03 TMT066              PIC 9(003).                                 
             03 TMT067              PIC 9(003).                                 
             03 TMT068              PIC 9(003).                                 
             03 TMT069              PIC 9(003).                                 
             03 TMT070              PIC 9(003).                                 
             03 TMT071              PIC 9(003).                                 
             03 TMT072              PIC 9(003).                                 
             03 TMT073              PIC 9(003).                                 
             03 TMT074              PIC 9(003).                                 
             03 TMT075              PIC 9(003).                                 
             03 TMT076              PIC 9(003).                                 
             03 TMT077              PIC 9(003).                                 
             03 TMT078              PIC 9(003).                                 
             03 TMT079              PIC 9(003).                                 
             03 TMT080              PIC 9(003).                                 
             03 TMT081              PIC 9(003).                                 
             03 TMT082              PIC 9(003).                                 
             03 TMT083              PIC 9(003).                                 
             03 TMT084              PIC 9(003).                                 
             03 TMT085              PIC 9(003).                                 
             03 TMT086              PIC 9(003).                                 
             03 TMT087              PIC 9(003).                                 
             03 TMT088              PIC 9(003).                                 
             03 TMT089              PIC 9(003).                                 
             03 TMT090              PIC 9(003).                                 
             03 TMT091              PIC 9(003).                                 
             03 TMT092              PIC 9(003).                                 
             03 TMT093              PIC 9(003).                                 
             03 TMT094              PIC 9(003).                                 
             03 TMT095              PIC 9(003).                                 
             03 TMT100              PIC 9(003).                                 
             03 TMT101              PIC 9(003).                                 
             03 TMT102              PIC 9(003).                                 
             03 TMT103              PIC 9(003).                                 
             03 TCP001              PIC 9(003).                                 
             03 TCP002              PIC 9(003).                                 
             03 TCP003              PIC 9(003).                                 
             03 TCP010              PIC 9(003).                                 
             03 TCP011              PIC 9(003).                                 
             03 TCP012              PIC 9(003).                                 
             03 TCP013              PIC 9(003).                                 
             03 TCP014              PIC 9(003).                                 
             03 TCP020              PIC 9(003).                                 
             03 TCP021              PIC 9(003).                                 
             03 TCP022              PIC 9(003).                                 
             03 TCP030              PIC 9(003).                                 
             03 TCP031              PIC 9(003).                                 
             03 TCP032              PIC 9(003).                                 
             03 TCP033              PIC 9(003).                                 
             03 TCP034              PIC 9(003).                                 
             03 TCP040              PIC 9(003).                                 
             03 TCP041              PIC 9(003).                                 
             03 TCP050              PIC 9(003).                                 
             03 TCP051              PIC 9(003).                                 
             03 TCP052              PIC 9(003).                                 
             03 TCP053              PIC 9(003).                                 
             03 TCP054              PIC 9(003).                                 
             03 TCP055              PIC 9(003).                                 
             03 TCP056              PIC 9(003).                                 
             03 TCP057              PIC 9(003).                                 
             03 TCP058              PIC 9(003).                                 
             03 TCP059              PIC 9(003).                                 
             03 TCP060              PIC 9(003).                                 
             03 TCP061              PIC 9(003).                                 
             03 TCP062              PIC 9(003).                                 
             03 TCP063              PIC 9(003).                                 
             03 TCP064              PIC 9(003).                                 
             03 TCP065              PIC 9(003).                                 
             03 TCP066              PIC 9(003).                                 
             03 TCP067              PIC 9(003).                                 
             03 TCP068              PIC 9(003).                                 
             03 TCP069              PIC 9(003).                                 
             03 TCP070              PIC 9(003).                                 
             03 TCP071              PIC 9(003).                                 
             03 TCP072              PIC 9(003).                                 
             03 TCP073              PIC 9(003).                                 
             03 TCP074              PIC 9(003).                                 
             03 TCP075              PIC 9(003).                                 
             03 TCP076              PIC 9(003).                                 
             03 TCP077              PIC 9(003).                                 
             03 TCP078              PIC 9(003).                                 
             03 TCP079              PIC 9(003).                                 
             03 TCP080              PIC 9(003).                                 
             03 TCP081              PIC 9(003).                                 
             03 TCP082              PIC 9(003).                                 
             03 TCP083              PIC 9(003).                                 
             03 TCP084              PIC 9(003).                                 
             03 TCP085              PIC 9(003).                                 
             03 TCP086              PIC 9(003).                                 
             03 TCP087              PIC 9(003).                                 
             03 TCP088              PIC 9(003).                                 
             03 TCP089              PIC 9(003).                                 
             03 TCP090              PIC 9(003).                                 
             03 TCP091              PIC 9(003).                                 
             03 TCP092              PIC 9(003).                                 
             03 TCP093              PIC 9(003).                                 
             03 TCP094              PIC 9(003).                                 
             03 TCP095              PIC 9(003).                                 
             03 TCP100              PIC 9(003).                                 
             03 TCP101              PIC 9(003).                                 
             03 TCP102              PIC 9(003).                                 
             03 TCP103              PIC 9(003).                                 
          02 FILLER REDEFINES TCAR-CARTERA.        
             03 V-TCARTERAS OCCURS 408 TIMES PIC 9(003).                        
          02 TCAR-104-105.                                                      
             03 TCR104              PIC 9(003).                                 
             03 TBS104              PIC 9(003).                                 
             03 TCC104              PIC 9(003).                                 
             03 TOT104              PIC 9(003).                                 
             03 TMT104              PIC 9(003).                                 
             03 TCP104              PIC 9(003).                                 
             03 TCR105              PIC 9(003).                                 
             03 TBS105              PIC 9(003).                                 
             03 TCC105              PIC 9(003).                                 
             03 TOT105              PIC 9(003).                                 
             03 TMT105              PIC 9(003).                                 
             03 TCP105              PIC 9(003).                                 
          02 TCAR-ENDEUDAMIENTO.        
             03 TGD002              PIC 9(003).        
             03 TGD010              PIC 9(007).        
             03 TGD011              PIC 9(007).        
             03 TGD012              PIC 9(007).        
             03 TGD022              PIC 9(003).                                 
             03 TGD023              PIC 9(003).                                 
             03 TGD024              PIC 9(003).        
             03 TGD032              PIC 9(003).                                 
             03 TGD033              PIC 9(003).                                 
             03 TGD034              PIC 9(003).        
             03 TGD042              PIC 9(003).                                 
             03 TGD043              PIC 9(003).                                 
             03 TGD044              PIC 9(003).        
             03 TGD050              PIC 9(003).        
          02 TCONSULTAS.        
             03 TIQ001              PIC 9(003).        
             03 TIQ002              PIC 9(003).        
             03 TIQ005              PIC 9(003).                                 
ACIC01       03 TIQ006              PIC 9(003).                         RMB0011
             03 TIQ012              PIC 9(003).                                 
             03 TIQ013              PIC 9(003).        
             03 TIQ014              PIC 9(003).        
             03 TIQ023              PIC 9(003).                                 
          02 TVALORES.                                                          
             03 TCR111              PIC 9(009).                                 
             03 TCR112              PIC 9(009).                                 
             03 TCR113              PIC 9(009).                                 
             03 TCR114              PIC 9(009).                                 
             03 TCR115              PIC 9(004)V99.                              
             03 TCR116              PIC 9(004)V99.                              
             03 TCR117              PIC 9(003).                                 
             03 TCR118              PIC 9(003).                                 
             03 TCR119              PIC 9(003).                                 
             03 TCR120              PIC 9(003).                                 
             03 TCR121              PIC 9(003).                                 
             03 TCR122              PIC 9(009).                                 
             03 TCR123              PIC 9(009).                                 
             03 TCR124              PIC 9(009).                                 
             03 TBS111              PIC 9(009).                                 
             03 TBS112              PIC 9(009).                                 
             03 TBS113              PIC 9(009).                                 
             03 TBS114              PIC 9(009).                                 
             03 TBS115              PIC 9(004)V99.                              
             03 TBS116              PIC 9(004)V99.                              
             03 TBS117              PIC 9(003).                                 
             03 TBS118              PIC 9(003).                                 
             03 TBS119              PIC 9(003).                                 
             03 TBS120              PIC 9(003).                                 
             03 TBS121              PIC 9(003).                                 
             03 TBS122              PIC 9(009).                                 
             03 TBS123              PIC 9(009).                                 
             03 TBS124              PIC 9(009).                                 
             03 TCC111              PIC 9(009).                                 
             03 TCC112              PIC 9(009).                                 
             03 TCC113              PIC 9(009).                                 
             03 TCC114              PIC 9(009).                                 
             03 TCC115              PIC 9(004)V99.                              
             03 TCC116              PIC 9(004)V99.                              
             03 TCC117              PIC 9(003).                                 
             03 TCC118              PIC 9(003).                                 
             03 TCC119              PIC 9(003).                                 
             03 TCC120              PIC 9(003).                                 
             03 TCC121              PIC 9(003).                                 
             03 TCC122              PIC 9(009).                                 
             03 TCC123              PIC 9(009).                                 
             03 TCC124              PIC 9(009).                                 
             03 TOT111              PIC 9(009).                                 
             03 TOT112              PIC 9(009).                                 
             03 TOT113              PIC 9(009).                                 
             03 TOT114              PIC 9(009).                                 
             03 TOT115              PIC 9(004)V99.                              
             03 TOT116              PIC 9(004)V99.                              
             03 TOT117              PIC 9(003).                                 
             03 TOT118              PIC 9(003).                                 
             03 TOT119              PIC 9(003).                                 
             03 TOT120              PIC 9(003).                                 
             03 TOT121              PIC 9(003).                                 
             03 TOT122              PIC 9(009).                                 
             03 TOT123              PIC 9(009).                                 
             03 TOT124              PIC 9(009).                                 
          02 TPROMEDIOS.                                                        
             03 TAT112              PIC 9(009).                                 
             03 TAT114              PIC 9(009).                                 
             03 TAT115              PIC 9(004)V99.                              
             03 TAT116              PIC 9(004)V99.                              
             03 TAT124              PIC 9(009).                                 
          02 THIPOTECARIOS.                                                     
             03 TMT111              PIC 9(009).                                 
             03 TMT112              PIC 9(009).                                 
             03 TMT113              PIC 9(009).                                 
             03 TMT114              PIC 9(009).                                 
             03 TMT115              PIC 9(004)V99.                              
             03 TMT116              PIC 9(004)V99.                              
             03 TMT117              PIC 9(003).                                 
             03 TMT118              PIC 9(003).                                 
             03 TMT119              PIC 9(003).                                 
             03 TMT120              PIC 9(003).                                 
             03 TMT121              PIC 9(003).                                 
             03 TMT122              PIC 9(009).                                 
             03 TMT123              PIC 9(009).                                 
             03 TMT124              PIC 9(009).                                 
       01 VARIABLES-CALCULO-SCORE.
             03 SCORE               PIC S9(005) VALUE 0.        
             03 A-SCORE             PIC S9(005) VALUE 0.        
       01 VARIABLES-INTERMEDIAS.        
          02 VARINT-PREDICTA.                                                   
             03 COBK                PIC S9(005).        
             03 SUM001              PIC S9(005).        
             03 AOT                 PIC S9(005).        
             03 AYT                 PIC S9(005).        
             03 AYTX                PIC S9(005).        
             03 BAL                 PIC S9(009).        
             03 BS25P7              PIC S9(005).        
             03 CBAT30              PIC S9(005).        
             03 CBAT39              PIC S9(005).        
             03 CBAT39X             PIC S9(005).        
             03 CBAT60P             PIC S9(005).        
             03 CBAT12W             PIC S9(005).        
             03 CR10B               PIC S9(005).        
      *      03 CRD13P              PIC S9(005).        
             03 CUAT69X             PIC S9(005).        
             03 DEL2P               PIC S9(005).        
             03 DEL2PX              PIC S9(005).        
             03 INQ06               PIC S9(005).        
             03 MALO1               PIC S9(005).        
             03 MESAS               PIC S9(005).        
             03 MOS30               PIC S9(005).        
             03 MOS69               PIC S9(005).        
             03 NREC                PIC S9(005).        
             03 OPAT12              PIC S9(005).        
             03 OPAT18              PIC S9(005).        
             03 OPAT24              PIC S9(005).        
             03 OPCR18              PIC S9(005).        
      *      03 OPOT24              PIC S9(005).        
             03 PCTCUR              PIC S9(005)V99.        
             03 SUM002              PIC S9(005).        
             03 SUM002X             PIC S9(005).        
             03 SUM050              PIC S9(005).        
      *      03 SUM054              PIC S9(005).        
             03 SUM064              PIC S9(005).        
             03 VRAT12              PIC S9(005).        
             03 VRCC12              PIC S9(005).        
             03 YOUNG               PIC S9(005).        
          02 VARINT-ACIERTA.                                                    
             03 TAT116-ROUNDED      PIC 9(004).                                 
             03 TCR115-ROUNDED      PIC 9(004).                                 
             03 A-SCORE-REAL        PIC S9(005).                                
             03 A-DERAT             PIC S9(005).                                
             03 A-SUM001            PIC S9(005).                                
             03 A-MOS120            PIC S9(005).                                
             03 A-DEFI              PIC S9(005).                                
             03 A-DIFF              PIC S9(005).                                
             03 A-NREC              PIC S9(005).                                
             03 A-CBAT60            PIC S9(005).                                
             03 A-CBAT90            PIC S9(005).                                
             03 A-CBAT120           PIC S9(005).                                
             03 A-CBAT60P           PIC S9(005).                                
             03 A-COBKAT            PIC S9(005).                                
             03 A-CUREAT            PIC S9(005).                                
             03 A-MOS30             PIC S9(005).                                
             03 A-DEL2P             PIC S9(005).                                
             03 A-PCTCUR            PIC S9(005).                                
             03 A-PCTDER            PIC S9(005).                                
             03 A-SUM002            PIC S9(005).                                
             03 A-SUM089            PIC S9(005).                                
             03 A-VRBS12            PIC S9(005).                                
             03 A-AOT               PIC S9(005).                                
             03 A-CBBS90P           PIC S9(005).                                
             03 A-MOSBS30P          PIC S9(005).                                
             03 A-MOSCC30P          PIC S9(005).                                
             03 A-MOSCR30P          PIC S9(005).                                
             03 A-NUMUTCR75         PIC S9(005).                                
             03 A-PCTCUROP          PIC S9(005).                                
             03 A-RECDEL            PIC S9(005).                                
             03 A-AYT               PIC S9(005).                                
             03 A-CBAT120P          PIC S9(005).                                
             03 A-CBBS39            PIC S9(005).                                
             03 A-CBCR30P           PIC S9(005).                                
             03 A-OPCC12            PIC S9(005).                                
             03 A-OPOT18            PIC S9(005).                                
             03 A-CBAT69            PIC S9(005).                                
             03 A-PCT69             PIC S9(005).                                
             03 A-INQ06             PIC S9(005).                                
             03 A-NUMUTBS25         PIC S9(005).                                
             03 A-PCTOP12           PIC S9(005).                                
             03 A-VRAT12            PIC S9(005).                                
             03 A-OPOT12            PIC S9(005).                                
             03 A-NAT112            PIC S9(005).                                
       01 VARIABLES-AA.        
             03 AAAOT                 PIC S9(005).        
             03 AAAYTX                PIC S9(005).        
             03 AAAYT                 PIC S9(005).        
             03 AABAL                 PIC S9(009).        
             03 AABS25P7              PIC S9(005).        
             03 AACBAT30              PIC S9(005).        
             03 AACBAT39X             PIC S9(005).        
             03 AACBAT60P             PIC S9(005).        
             03 AACBAT12W             PIC S9(005).        
             03 AACR10B               PIC S9(005).        
      *      03 AACRD13P              PIC S9(005).        
             03 AACUAT69X             PIC S9(005).        
             03 AADEL2PX              PIC S9(005).        
             03 AAINQ06               PIC S9(005).        
             03 AAMALO1               PIC S9(005).        
             03 AAMOS30               PIC S9(005).        
             03 AAMOS69               PIC S9(005).        
             03 AANREC                PIC S9(005).        
             03 AAOPAT12              PIC S9(005).        
             03 AAOPAT18              PIC S9(005).        
             03 AAOPAT24              PIC S9(005).        
             03 AAOPCR18              PIC S9(005).        
      *      03 AAOPOT24              PIC S9(005).        
             03 AAPCTCUR              PIC S9(005)V99.        
             03 AASUM002X             PIC S9(005).        
             03 AASUM050              PIC S9(005).        
      *      03 AASUM054              PIC S9(005).        
             03 AASUM064              PIC S9(005).        
             03 AAVRAT12              PIC S9(005).        
             03 AAVRCC12              PIC S9(005).        
       01 VARIABLES-ECUACION.        
          02 VARECU-PREDICTA.                                                   
             03 CC103001            PIC 9(005).        
             03 CC103002            PIC 9(005).        
             03 CR040001            PIC 9(005).        
             03 CR040002            PIC 9(005).        
             03 CR040003            PIC 9(005).        
             03 CR103001            PIC 9(005).        
             03 CR103002            PIC 9(005).        
             03 NREC0001            PIC 9(005).        
      *      03 OPOT2401            PIC 9(005).        
             03 SUM06401            PIC 9(005).        
             03 BS067001            PIC 9(005).        
             03 MALO1001            PIC 9(005).        
             03 MALO1002            PIC 9(005).        
             03 VRAT1201            PIC 9(005).        
             03 MOS30001            PIC 9(005).        
             03 CBAT60P1            PIC 9(005).        
             03 BAL00001            PIC 9(005).        
             03 BAL00002            PIC 9(005).        
             03 BAL00003            PIC 9(005).        
      *      03 CK001001            PIC 9(005).        
             03 INQ06001            PIC 9(005).        
             03 INQ06002            PIC 9(005).        
             03 INQ06003            PIC 9(005).        
             03 OPAT1801            PIC 9(005).        
             03 OPAT1802            PIC 9(005).        
             03 BS040001            PIC 9(005).        
             03 AOT00001            PIC 9(005).        
             03 AYT00001            PIC 9(005).        
             03 AYT00002            PIC 9(005).        
             03 BS25P701            PIC 9(005).        
             03 CBAT3001            PIC 9(005).        
             03 CBAT3002            PIC 9(005).        
             03 CC001001            PIC 9(005).        
      *      03 CK001002            PIC 9(005).        
             03 CR041001            PIC 9(005).        
             03 GD002001            PIC 9(005).        
             03 IQ013001            PIC 9(005).        
             03 IQ014001            PIC 9(005).        
             03 OPAT1201            PIC 9(005).        
             03 OPAT1202            PIC 9(005).        
             03 OPAT1203            PIC 9(005).        
             03 OPCR1801            PIC 9(005).        
             03 OPCR1802            PIC 9(005).        
             03 GD050001            PIC 9(005).        
             03 OPAT2401            PIC 9(005).        
             03 PCTCUR01            PIC 9(005).        
             03 PCTCUR02            PIC 9(005).        
             03 SUM05001            PIC 9(005).        
             03 SUM05002            PIC 9(005).        
      *      03 SUM05401            PIC 9(005).        
      *      03 CRD13P01            PIC 9(005).        
             03 MOS69001            PIC 9(005).        
             03 MOS69002            PIC 9(005).        
             03 MOS69003            PIC 9(005).        
             03 MOS69004            PIC 9(005).        
             03 CBAT12W1            PIC 9(005).        
             03 CBAT12W2            PIC 9(005).        
             03 BS002001            PIC 9(005).        
             03 VRCC1201            PIC 9(005).        
             03 CR001001            PIC 9(005).        
             03 CR10B001            PIC 9(005).        
          02 VARECU-ACIERTA.                                                    
             03 A-ID004X            PIC 9(005).                                 
             03 A-BS055001          PIC 9(005).                                 
             03 A-BS055002          PIC 9(005).                                 
             03 A-BS090001          PIC 9(005).                                 
             03 A-BS093001          PIC 9(005).                                 
             03 A-BS122001          PIC 9(005).                                 
             03 A-CC093001          PIC 9(005).                                 
             03 A-CR093001          PIC 9(005).                                 
             03 A-CR122001          PIC 9(005).                                 
             03 A-CUREAT01          PIC 9(005).                                 
             03 A-IQ014001          PIC 9(005).                                 
             03 A-MT122001          PIC 9(005).                                 
             03 A-PCTCUR01          PIC 9(005).                                 
             03 A-PCTCUR02          PIC 9(005).                                 
             03 A-PCTCUR03          PIC 9(005).                                 
             03 A-PCTCUR04          PIC 9(005).                                 
             03 A-PCTCUR05          PIC 9(005).                                 
             03 A-PCTCUR06          PIC 9(005).                                 
             03 A-PCTDER01          PIC 9(005).                                 
             03 A-SUM00201          PIC 9(005).                                 
             03 A-SUM00202          PIC 9(005).                                 
             03 A-SUM00203          PIC 9(005).                                 
             03 A-VRBS1201          PIC 9(005).                                 
             03 A-SUM08901          PIC 9(005).                                 
             03 A-CBAT60P1          PIC 9(005).                                 
             03 A-CBAT60P2          PIC 9(005).                                 
             03 A-CBAT60P3          PIC 9(005).                                 
             03 A-COBKAT01          PIC 9(005).                                 
             03 A-COBKAT02          PIC 9(005).                                 
             03 A-COBKAT03          PIC 9(005).                                 
             03 A-COBKAT04          PIC 9(005).                                 
             03 A-DEL2P01           PIC 9(005).                                 
             03 A-MOS3001           PIC 9(005).                                 
             03 A-MOS3002           PIC 9(005).                                 
             03 A-MOS3003           PIC 9(005).                                 
             03 A-OT067001          PIC 9(005).                                 
             03 A-OT112001          PIC 9(005).                                 
             03 A-BS124001          PIC 9(005).                                 
             03 A-MOSCC30P1         PIC 9(005).                                 
             03 A-MOSCC30P2         PIC 9(005).                                 
             03 A-MOSBS30P1         PIC 9(005).                                 
             03 A-MOSBS30P2         PIC 9(005).                                 
             03 A-MOSBS30P3         PIC 9(005).                                 
             03 A-MOSCR30P1         PIC 9(005).                                 
             03 A-PCTCUROP1         PIC 9(005).                                 
             03 A-PCTCUROP2         PIC 9(005).                                 
             03 A-PCTCUROP3         PIC 9(005).                                 
             03 A-CBBS90P1          PIC 9(005).                                 
             03 A-NUMUTCR751        PIC 9(005).                                 
             03 A-AOT00001          PIC 9(005).                                 
             03 A-RECDEL01          PIC 9(005).                                 
             03 A-RECDEL02          PIC 9(005).                                 
             03 A-RECDEL03          PIC 9(005).                                 
             03 A-RECDEL04          PIC 9(005).                                 
             03 A-RECDEL05          PIC 9(005).                                 
             03 A-RECDEL06          PIC 9(005).                                 
             03 A-MESES             PIC 9(005).                                 
             03 A-AT116001          PIC 9(005).                                 
             03 A-AYT00001          PIC 9(005).                                 
             03 A-AYT00002          PIC 9(005).                                 
             03 A-BS111001          PIC 9(005).                                 
             03 A-BS121001          PIC 9(005).                                 
             03 A-CBAT120P1         PIC 9(005).                                 
             03 A-CBBS3901          PIC 9(005).                                 
             03 A-CBCR30P1          PIC 9(005).                                 
             03 A-CR030001          PIC 9(005).                                 
             03 A-IQ012001          PIC 9(005).                                 
             03 A-IQ012002          PIC 9(005).                                 
             03 A-OPCC1201          PIC 9(005).                                 
             03 A-OPOT1801          PIC 9(005).                                 
             03 A-PCT69001          PIC 9(005).                                 
             03 A-AOTX              PIC 9(005).                                 
             03 A-CBAT60PX          PIC 9(005).                                 
             03 A-BS123001          PIC 9(005).                                 
             03 A-BS123002          PIC 9(005).                                 
             03 A-BS123003          PIC 9(005).                                 
             03 A-CR111001          PIC 9(005).                                 
             03 A-CR111002          PIC 9(005).                                 
             03 A-OT111001          PIC 9(005).                                 
             03 A-VRAT1201          PIC 9(005).                                 
             03 A-INQ06001          PIC 9(005).                                 
             03 A-OT041001          PIC 9(005).                                 
             03 A-PCTOP121          PIC 9(005).                                 
             03 A-NUMUTBS251        PIC 9(005).                                 
             03 A-AGE               PIC 9(005).                                 
             03 A-RECDELAA          PIC 9(005).                                 
             03 A-SUM00101          PIC 9(005).                                 
             03 A-AT112001          PIC 9(005).                                 
             03 A-BS064001          PIC 9(005).                                 
             03 A-CC020001          PIC 9(005).                                 
             03 A-CC040001          PIC 9(005).                                 
             03 A-CR115001          PIC 9(005).                                 
             03 A-IQ013001          PIC 9(005).                                 
             03 A-IQ013002          PIC 9(005).                                 
             03 A-NAT11201          PIC 9(005).                                 
             03 A-OPOT1201          PIC 9(005).                                 
DELTA0 01 VARIABLES-DELTA.                                              RMB0012
DELTA0    02 HACER-SCO-DELTA        PIC 9 VALUE 0.                      RMB0012
DELTA0    02 SCORE-DELTA OCCURS 10 TIMES PIC 9(3).                      RMB0012
DELTA0    02 RAZONES-DELTA OCCURS 10 TIMES.                             RMB0012
DELTA0       03 ADVERSE-RAZON1-DELTA PIC 99.                            RMB0012
DELTA0       03 ADVERSE-RAZON2-DELTA PIC 99.                            RMB0012
DELTA0       03 ADVERSE-RAZON3-DELTA PIC 99.                            RMB0012
DELTA0       03 ADVERSE-RAZON4-DELTA PIC 99.                            RMB0012
DELTA0    02 AMNISTIA21-LEEMSTRS-AUX PIC 9 VALUE 0.                     RMB0012
ARTI13    02 SW-ART13-AMNIST21-AUX   PIC 9 VALUE 0.                     GJC0014
ARTI13    02 SW-NOV-48-AUX           PIC 9 VALUE 0.                     GJC0014
       01 VARIABLES-ADVERSAS.        
           02 ADV-PREDICTA.                                                     
             03 RAZONES             PIC 99.        
             03 ADVERSE-CODE        PIC 99.        
             03 ADVERSE-CODE-INI    PIC 99.        
             03 ADVERSE-CODE-AUX    PIC 99.        
             03 DIFFERENCE          PIC 9(5).        
             03 DIFFERENCE-AUX      PIC 9(5).        
             03 TEMP1               PIC S9(5)V99.        
             03 TEMP1ROUNDED        PIC S9(5).        
             03 V-ADVERSOS.        
                04 ADVERSE-RAZON OCCURS 4 TIMES PIC 99.        
                04 ADVERSE-DIFF  OCCURS 4 TIMES PIC 9(5).        
                04 V-ASIGNADA    OCCURS 32 TIMES PIC 9.        
             03 RAZONES-APLHA.        
                04 FILLER PIC X(55) VALUE        
            "MORA EN CUENTA DE CELULARES/COMUNICACIONES D           ".          
             03 ADVERSE-RAZON-ALPHA OCCURS 32 TIMES PIC X(60).        
           02 ADV-ACIERTA  .                                                    
             03 A-RAZONES             PIC 99.                                   
             03 A-ADVERSE-CODE        PIC 99.                                   
             03 A-ADVERSE-CODE-INI    PIC 99.                                   
             03 A-ADVERSE-CODE-AUX    PIC 99.                                   
             03 A-DIFFERENCE          PIC 9(5)V999999.                          
             03 A-DIFFERENCE-AUX      PIC 9(5)V999999.                          
             03 A-TEMP1               PIC S9(5)V999999.                         
             03 A-TEMP1ROUNDED        PIC S9(5).                                
             03 A-V-ADVERSOS.                                                   
                04 A-ADVERSE-RAZON OCCURS 4 TIMES PIC 99.                       
                04 A-ADVERSE-RAZON-REAL OCCURS 4 TIMES PIC 99.                  
                04 A-ADVERSE-DIFF  OCCURS 4 TIMES PIC 9(5)V999999.              
                04 A-V-ASIGNADA    OCCURS 50 TIMES PIC 9.                       
             03 A-RAZONES-APLHA.                                                
                04 FILLER PIC X(55) VALUE                                       
            "MORA EN CUENTA DE CELULARES/COMUNICACIONES D           ".          
             03 A-ADVERSE-RAZON-ALPHA OCCURS 50 TIMES PIC X(60).                
           02 ADV-ACIERTA-PLUS.                                                 
             03 P-RAZONES             PIC 99.                                   
             03 P-ADVERSE-CODE        PIC 99.                                   
             03 P-ADVERSE-CODE-INI    PIC 99.                                   
             03 P-ADVERSE-CODE-AUX    PIC 99.                                   
             03 P-DIFFERENCE          PIC 9(5)V999999.                          
             03 P-DIFFERENCE-AUX      PIC 9(5)V999999.                          
             03 P-TEMP1               PIC S9(5)V999999.                         
             03 P-TEMP1ROUNDED        PIC S9(5).                                
             03 P-V-ADVERSOS.                                                   
                04 P-ADVERSE-RAZON OCCURS 4 TIMES PIC 99.                       
                04 P-ADVERSE-RAZON-REAL OCCURS 4 TIMES PIC 99.                  
                04 P-ADVERSE-DIFF  OCCURS 4 TIMES PIC 9(5)V999999.              
                04 P-V-ASIGNADA    OCCURS 100 TIMES PIC 9.                      
             03 P-RAZONES-APLHA.                                                
                04 FILLER PIC X(55) VALUE                                       
            "MORA EN CUENTA DE CELULARES/COMUNICACIONES D           ".          
             03 P-ADVERSE-RAZON-ALPHA OCCURS 100 TIMES PIC X(60).               
       01 VECTORES-GENERALES.        
          02 OPEN-DATE-CAR  OCCURS 6 TIMES.                                     
             03 V-OPE-DATE  OCCURS 500 TIMES PIC 9(6).        
             03 V-NUM-CAR                    PIC 9(3).        
      *** 1=CR,2=BS,3=CC,4=OT        
          02 TOT-WORK OCCURS 6 TIMES.                                           
             03 V-WORK       OCCURS 120 TIMES PIC 9(3).        
          02 T-CONDADORES.                                                      
             03 FILLER OCCURS 6 TIMES.                                          
                05 CON-CTA-BAL      PIC 9(6) VALUE 0.                           
                05 CON-CTA-LIM      PIC 9(6) VALUE 0.                           
                05 CON-CTA-BALLIM   PIC 9(6) VALUE 0.                           
                05 CON-CTA-DEL      PIC 9(6) VALUE 0.                           
                05 VAL-CTA-BAL      PIC 9(9) VALUE 0.                           
                05 VAL-CTA-LIM      PIC 9(9) VALUE 0.                           
                05 VAL-CTA-BALLIM   PIC 9(9)V9(9) VALUE 0.                      
             03 CON-TOT-BAL      PIC 9(6) VALUE 0.                              
             03 CON-TOT-LIM      PIC 9(6) VALUE 0.                              
             03 CON-TOT-BALLIM   PIC 9(6) VALUE 0.                              
             03 CON-TOT-DEL      PIC 9(6) VALUE 0.                              
             03 VAL-TOT-BAL      PIC 9(9).                                      
             03 VAL-TOT-LIM      PIC 9(9).                                      
             03 VAL-TOT-BAL0     PIC 9(9).                                      
             03 VAL-TOT-LIM0     PIC 9(9).                                      
             03 VAL-TOT-DEL      PIC 9(9).                                      
             03 VAL-TOT-BALLIM   PIC 9(9)V9(9) VALUE 0.                         
             03 CON-HIP-BAL      PIC 9(6) VALUE 0.                              
             03 CON-HIP-LIM      PIC 9(6) VALUE 0.                              
             03 CON-HIP-BALLIM   PIC 9(6) VALUE 0.                              
             03 CON-HIP-DEL      PIC 9(6) VALUE 0.                              
             03 VAL-HIP-BAL      PIC 9(9).                                      
             03 VAL-HIP-LIM      PIC 9(9).                                      
             03 VAL-HIP-BAL0     PIC 9(9).                                      
             03 VAL-HIP-LIM0     PIC 9(9).                                      
             03 VAL-HIP-DEL      PIC 9(9).                                      
             03 VAL-HIP-BALLIM   PIC 9(9)V9(9) VALUE 0.                         
          02 V-FLAGS-CAR   OCCURS 6 TIMES.                                      
             03 CTAFLAG           PIC 9.        
             03 OPEFLAG           PIC 9.        
             03 DDFLAG            PIC 9.        
             03 CLOFLAG           PIC 9.        
             03 DOFLAG            PIC 9.        
             03 VFLAG             PIC 9.                                        
             03 CBFLAG            PIC 9.                                        
             03 CLFLAG            PIC 9.                                        
             03 UTFLAG            PIC 9.                                        
             03 EXFLAG            PIC 9.                                        
             03 DQFLAG            PIC 9.                                        
             03 PMTFLAG           PIC 9.                                        
          02 FLAGS-TOT.                                                         
             03 CTAFLAG-TOT       PIC 9.                                        
             03 OPEFLAG-TOT       PIC 9.                                        
             03 DDFLAG-TOT        PIC 9.                                        
             03 VFLAG-TOT         PIC 9.                                        
             03 CBFLAG-TOT        PIC 9.                                        
             03 CLFLAG-TOT        PIC 9.                                        
             03 UTFLAG-TOT        PIC 9.                                        
             03 EXFLAG-TOT        PIC 9.                                        
             03 DQFLAG-TOT        PIC 9.                                        
          02 FLAGS-HIP.                                                         
             03 CTAFLAG-HIP       PIC 9.                                        
             03 OPEFLAG-HIP       PIC 9.                                        
             03 DDFLAG-HIP        PIC 9.                                        
             03 CLOFLAG-HIP       PIC 9.                                        
             03 DOFLAG-HIP        PIC 9.                                        
             03 VFLAG-HIP         PIC 9.                                        
             03 CBFLAG-HIP        PIC 9.                                        
             03 CLFLAG-HIP        PIC 9.                                        
             03 UTFLAG-HIP        PIC 9.                                        
             03 EXFLAG-HIP        PIC 9.                                        
             03 DQFLAG-HIP        PIC 9.                                        
             03 PMTFLAG-HIP       PIC 9.                                        
          02 V-FLAGS-GD  .        
             03 QTRFLAG           PIC 9.        
             03 TIPFLAG           PIC 9 OCCURS 3 TIMES.        
          02 V-FLAGS-IQ.        
             03 IQFLAG            PIC 9.        
             03 CIFLAG            PIC 9.                                        
             03 TIPIQFLAG         PIC 9 OCCURS 5 TIMES. *> MAS HIP              
          02 V-FLAGS-CK.        
             03 CKFLAG            PIC 9.        
             03 CKDOFLAG          PIC 9.                                        
             03 AHFLAG            PIC 9.                                        
             03 AHDOFLAG          PIC 9.                                        
          02 VALORES-OK-FLAG      PIC 9 VALUE 0.                                
          02 VALOR-MOR-OK-FLAG    PIC 9 VALUE 0.                                
       01 VARIABLES-ACI-NUMERICAS.                                              
REMARK* ESTAS VARIABLES SE USAN PARA MANEJAR EL PUNTO DECIMAL Y NO
REMARK* DANAR EL VALOR ORIGINAL DEL ARCHIVO QUE SI TIENE EL PUNTO
          02 CR115-NUM            PIC 9(6) VALUE 0.                             
          02 CR116-NUM            PIC 9(6) VALUE 0.                             
          02 BS116-NUM            PIC 9(6) VALUE 0.                             
          02 AT116-NUM            PIC 9(6) VALUE 0.                             
          02 MT115-NUM            PIC 9(6) VALUE 0.                             
       01 VECTOR-VALIDO.        
          02 FILLER PIC X(30) VALUE "111000000111110000011100000001".        
          02 FILLER PIC X(30) VALUE "111100000110000000011111111111".        
          02 FILLER PIC X(30) VALUE "111111111111111111111111111111".        
          02 FILLER PIC X(30) VALUE "111110000111100000000000000000".        
       01 FILLER REDEFINES VECTOR-VALIDO.        
          02 V-VALIDO        PIC 9  OCCURS 120 TIMES.        
       01 VARIABLES-GENERALES.        
          02 UTIL-AUX                    PIC 9(9)V99.                           
          02 VALOR-AUX.                                                         
             03 VAL-ENTERO               PIC 9(3).                              
             03 FILLER                   PIC X(1) VALUE ".".                    
             03 VAL-DECIMAL              PIC 9(2).                              
          02 FECHA-AUX-SCO               PIC 9(4).        
          02 FECHA-AUX6                  PIC 9(6).        
          02 FECHA-RUN-DATE              PIC 9(6).        
          02 FILLER REDEFINES FECHA-RUN-DATE.         
             03 SIG-ANO-RUN              PIC 9(4).        
             03 FILLER REDEFINES SIG-ANO-RUN.        
                04 SIG-RUN               PIC 9(2).        
                04 ANO-RUN               PIC 9(2).        
             03 MES-RUN                  PIC 9(2).        
          02 FECHA-NOVEDAD07             PIC 9(6).        
          02 FILLER REDEFINES FECHA-NOVEDAD07.        
             03 SIG-ANO-NOV              PIC 9(4).        
             03 FILLER REDEFINES SIG-ANO-NOV.        
                04 SIG-NOV               PIC 9(2).        
                04 ANO-NOV               PIC 9(2).        
             03 MES-NOV                  PIC 9(2).        
          02 FECHA-APERTURA07            PIC 9(6).        
          02 FILLER REDEFINES FECHA-APERTURA07.        
             03 SIG-ANO-APE              PIC 9(4).        
             03 FILLER REDEFINES SIG-ANO-APE.        
                04 SIG-APE               PIC 9(2).        
                04 ANO-APE               PIC 9(2).        
             03 MES-APE                  PIC 9(2).        
          02 FECHA-VENCIMIENTO07         PIC 9(6).        
          02 FILLER REDEFINES FECHA-VENCIMIENTO07.        
             03 SIG-ANO-VEN              PIC 9(4).        
             03 FILLER REDEFINES SIG-ANO-VEN.        
                04 SIG-VEN               PIC 9(2).        
                04 ANO-VEN               PIC 9(2).        
             03 MES-VEN                  PIC 9(2).        
          02 FECHA-NOVEDAD07-AUX         PIC 9(6).                              
          02 FECHA-VALORES07             PIC 9(6).                              
          02 FILLER REDEFINES FECHA-VALORES07.                                  
             03 SIG-ANO-VAL              PIC 9(4).                              
             03 FILLER REDEFINES SIG-ANO-VAL.                                   
                04 SIG-VAL               PIC 9(2).                              
                04 ANO-VAL               PIC 9(2).                              
             03 MES-VAL                  PIC 9(2).                              
          02 FECHA-SITUACION07           PIC 9(6).                              
          02 FILLER REDEFINES FECHA-SITUACION07.                                
             03 SIG-ANO-SIT              PIC 9(4).                              
             03 FILLER REDEFINES SIG-ANO-SIT.                                   
                04 SIG-SIT               PIC 9(2).                              
                04 ANO-SIT               PIC 9(2).                              
             03 MES-SIT                  PIC 9(2).                              
          02 FECHA-CASTIGADA07           PIC 9(6).                              
          02 FILLER REDEFINES FECHA-CASTIGADA07.                                
             03 SIG-ANO-CAS              PIC 9(4).                              
             03 FILLER REDEFINES SIG-ANO-CAS.                                   
                04 SIG-CAS               PIC 9(2).                              
                04 ANO-CAS               PIC 9(2).                              
             03 MES-CAS                  PIC 9(2).                              
          02 FECHA-ACTSUS                PIC 9(6).        
          02 FILLER REDEFINES FECHA-ACTSUS.             
             03 SIG-ANO-SUS              PIC 9(4).        
             03 FILLER REDEFINES SIG-ANO-SUS.        
                04 SIG-SUS               PIC 9(2).        
                04 ANO-SUS               PIC 9(2).        
             03 MES-SUS                  PIC 9(2).        
          02 FECHA-INGSUS                PIC 9(6).        
          02 FILLER REDEFINES FECHA-INGSUS.            
             03 SIG-ANO-ING              PIC 9(4).        
             03 FILLER REDEFINES SIG-ANO-ING.        
                04 SIG-ING               PIC 9(2).        
                04 ANO-ING               PIC 9(2).        
             03 MES-ING                  PIC 9(2).        
          02 FECHA-CON-INDE              PIC 9(6).        
          02 FILLER REDEFINES FECHA-CON-INDE.         
             03 SIG-ANO-CONSU            PIC 9(4).        
             03 FILLER REDEFINES SIG-ANO-CONSU.        
                04 SIG-CONSU             PIC 9(2).        
                04 ANO-CONSU             PIC 9(2).        
             03 MES-CONSU                PIC 9(2).        
          02 FECHA-EXP-CED               PIC 9(6).        
          02 FILLER REDEFINES FECHA-EXP-CED .         
             03 SIG-ANO-CED              PIC 9(4).        
             03 FILLER REDEFINES SIG-ANO-CED.        
                04 SIG-CED               PIC 9(2).        
                04 ANO-CED               PIC 9(2).        
             03 MES-CED                  PIC 9(2).        
          02 ULTIMO-TRI                  PIC 9(8).        
          02 ULTIMO-TRI-AUX              PIC 9(8).                              
          02 TIP                         PIC 9(8).        
          02 CAL-SCO                     PIC 9(8).        
          02 FIN-MSUP                    PIC 9(01) VALUE 0.        
          02 W-VALOR-SCO                 PIC 9(14) VALUE 0.        
          02 FIN-CON                     PIC 9(01) VALUE 0.        
          02 FIN-CHK                     PIC 9(01) VALUE 0.        
          02 VALIDA                      PIC 9 VALUE 0.        
          02 POS                         PIC 9(8).        
          02 MESES-MORA                  PIC 9(8).        
          02 TIPO                        PIC 9(8).        
          02 TIPO-AUX                    PIC 9(8).                              
          02 DES-TIPO                    PIC X(02).                             
          02 OLD-MORTAGE                 PIC 9(01).                             
          02 MORA                        PIC 9(8).        
          02 TIP-IDE-SCO                 PIC 9.        
          02 NUM-IDE-SCO                 PIC 9(11).        
          02 SCORE-CARD                  PIC 9 VALUE 0.        
          02 A-SCORE-CARD                PIC 9 VALUE 0.                         
          02 M-TIPCAR-PLUS.                                                     
             03 FILLER   PIC X(12) VALUE "CRBSCCOTMTCP".                        
          02 FILLER REDEFINES M-TIPCAR-PLUS.                                    
             03 V-TIPCAR-PLUS OCCURS 6 TIMES PIC X(02).                         
          02 VECTOR-MORA.        
             03 FILLER PIC X(25) VALUE "5999999999995555123411212".        
             03 FILLER PIC X(25) VALUE "3234124412341234999494999".        
          02 FILLER REDEFINES VECTOR-MORA.        
             03 V-MORA OCCURS 50 TIMES PIC 9.        
          02 TIP-CTA.                 
      *******2=CK,1=CR,2=BS,3=CC,4=OT,9=INVALIDO        
             03 FILLER PIC X(30) VALUE "211422244444224442444433442444".        
             03 FILLER PIC X(30) VALUE "444444444444444444499999999999".        
             03 FILLER PIC X(30) VALUE "999999999999999999999999999999".        
             03 FILLER PIC X(10) VALUE "9999999999".        
          02 FILLER REDEFINES TIP-CTA.                 
             03 V-TIPCTA  OCCURS 100 TIMES PIC 9.        
          02 TIP-CRE.                                                           
      *******I=INSTALMMENT,M=MORTGAGES,N=OTHERS,E=SPECIAL CASE(I and M)         
             03 FILLER PIC X(30) VALUE "NNNIEIIIIIIIMIIIIIIIIINNIIIINI".        
             03 FILLER PIC X(30) VALUE "IIIIIIIIIIIIIIIIIIINNNNNNNNNNN".        
             03 FILLER PIC X(30) VALUE "NNNNNNNNNNNNNNNNNNNNNNNNNNNNNN".        
             03 FILLER PIC X(10) VALUE "NNNNNNNNNN".                            
          02 FILLER REDEFINES TIP-CRE.                 
             03 V-TIPCRE  OCCURS 100 TIMES PIC X.        
          02 VECTOR-HIS                  PIC 9(12).        
          02 FILLER REDEFINES VECTOR-HIS.        
             03 V-HIS         OCCURS 12 TIMES PIC 9.        
       01 VARIABLES-PROPIAS.        
          02 TIP-SCO-GENERICO            PIC 9 VALUE 1.                         
          02 CED-COMPLETA.        
             03 CED-TIPIDE               PIC 9.        
             03 CED-NUMIDE               PIC 9(11).        
          02 TIP-ID-ANT                  PIC 9.        
          02 CEDULA-ANT                  PIC 9(11).        
          02 PER                         PIC 9(08) VALUE 0.        
          02 FILLER REDEFINES PER.        
             03 FILLER                   PIC 9(05).        
             03 DIEZMIL                  PIC 9(03).        
          02 FILLER REDEFINES PER.                                              
             03 FILLER                   PIC 9(04).                             
             03 CIENMIL                  PIC 9(05).                             
          02 FIN-CTA                     PIC 9(01) VALUE 0.        
          02 FIN-CAR                     PIC 9(01) VALUE 0.        
       01 CRITERIO-DE-EXCLUSION          pic 9 value 0.                         
       01 VARIABLES-MICROCREDITO-ZERO.                                          
          02 M-SCORE-CARD                PIC 9 .                                
          02 M-ADVERSE-RAZON OCCURS 4 TIMES PIC 99.                             
          02 M-MOS60                     PIC S9(005).                           
          02 M-SCORE                     PIC S9(005).                           
          02 M-AYT-1                     PIC 9 VALUE 0.                         
          02 M-AOT-1                     PIC 9 VALUE 0.                         
          02 M-SUM002-1                  PIC 9 VALUE 0.                         
          02 M-RECDEL-1                  PIC 9 VALUE 0.                         
          02 M-RECDEL-2                  PIC 9 VALUE 0.                         
          02 M-RECDEL-3                  PIC 9 VALUE 0.                         
          02 M-RECDEL-4                  PIC 9 VALUE 0.                         
          02 M-RECDEL-5                  PIC 9 VALUE 0.                         
          02 M-RECDEL-6                  PIC 9 VALUE 0.                         
          02 M-RECDEL-7                  PIC 9 VALUE 0.                         
          02 M-AT112-1                   PIC 9 VALUE 0.                         
          02 M-AT116-1                   PIC 9 VALUE 0.                         
          02 M-AT124-4                   PIC 9 VALUE 0.                         
          02 M-CK001-1                   PIC 9 VALUE 0.                         
          02 M-CK002-1                   PIC 9 VALUE 0.                         
          02 M-BS002-1                   PIC 9 VALUE 0.                         
          02 M-BS002-2                   PIC 9 VALUE 0.                         
          02 M-BS002-3                   PIC 9 VALUE 0.                         
          02 M-BS012-1                   PIC 9 VALUE 0.                         
          02 M-BS041-1                   PIC 9 VALUE 0.                         
          02 M-BS057-1                   PIC 9 VALUE 0.                         
          02 M-BS059-1                   PIC 9 VALUE 0.                         
          02 M-BS061-1                   PIC 9 VALUE 0.                         
          02 M-BS124-1                   PIC 9 VALUE 0.                         
          02 M-BS124-2                   PIC 9 VALUE 0.                         
          02 M-BS124-3                   PIC 9 VALUE 0.                         
          02 M-CBBS39-1                  PIC 9 VALUE 0.                         
          02 M-CC002-1                   PIC 9 VALUE 0.                         
          02 M-CC010-1                   PIC 9 VALUE 0.                         
          02 M-CC050-1                   PIC 9 VALUE 0.                         
          02 M-CC123-1                   PIC 9 VALUE 0.                         
          02 M-CC123-2                   PIC 9 VALUE 0.                         
          02 M-CC123-3                   PIC 9 VALUE 0.                         
          02 M-CR030-1                   PIC 9 VALUE 0.                         
          02 M-CR031-1                   PIC 9 VALUE 0.                         
          02 M-CR061-1                   PIC 9 VALUE 0.                         
          02 M-CR111-1                   PIC 9 VALUE 0.                         
          02 M-CR116-3                   PIC 9 VALUE 0.                         
          02 M-CR123-1                   PIC 9 VALUE 0.                         
          02 M-CR123-2                   PIC 9 VALUE 0.                         
          02 M-CR123-3                   PIC 9 VALUE 0.                         
          02 M-CR123-4                   PIC 9 VALUE 0.                         
          02 M-ID004-1                   PIC 9 VALUE 0.                         
          02 M-ID004-2                   PIC 9 VALUE 0.                         
          02 M-ID004-3                   PIC 9 VALUE 0.                         
          02 M-ID004-4                   PIC 9 VALUE 0.                         
          02 M-ID004-5                   PIC 9 VALUE 0.                         
          02 M-ID004-6                   PIC 9 VALUE 0.                         
          02 M-ID004-7                   PIC 9 VALUE 0.                         
          02 M-DEL2P-1                   PIC 9 VALUE 0.                         
          02 M-DERAT-1                   PIC 9 VALUE 0.                         
          02 M-IQ001-1                   PIC 9 VALUE 0.                         
          02 M-IQ001-2                   PIC 9 VALUE 0.                         
          02 M-IQ001-3                   PIC 9 VALUE 0.                         
          02 M-IQ001-4                   PIC 9 VALUE 0.                         
          02 M-IQ002-1                   PIC 9 VALUE 0.                         
          02 M-IQ002-2                   PIC 9 VALUE 0.                         
          02 M-IQ002-3                   PIC 9 VALUE 0.                         
          02 M-IQ005-1                   PIC 9 VALUE 0.                         
          02 M-IQ005-4                   PIC 9 VALUE 0.                         
          02 M-IQ005-5                   PIC 9 VALUE 0.                         
          02 M-IQ005-6                   PIC 9 VALUE 0.                         
          02 M-IQ005-7                   PIC 9 VALUE 0.                         
          02 M-IQ013-1                   PIC 9 VALUE 0.                         
          02 M-IQ013-2                   PIC 9 VALUE 0.                         
          02 M-IQ023-1                   PIC 9 VALUE 0.                         
          02 M-MOS30-12                  PIC 9 VALUE 0.                         
          02 M-MOS30-3                   PIC 9 VALUE 0.                         
          02 M-MOS60-1                   PIC 9 VALUE 0.                         
          02 M-MOS60-2                   PIC 9 VALUE 0.                         
          02 M-MOS60-3                   PIC 9 VALUE 0.                         
          02 M-MOSBS30P-9                PIC 9 VALUE 0.                         
          02 M-OPCC12-1                  PIC 9 VALUE 0.                         
          02 M-OPCC12-2                  PIC 9 VALUE 0.                         
          02 M-OPCC12-3                  PIC 9 VALUE 0.                         
          02 M-OPOT12-1                  PIC 9 VALUE 0.                         
          02 M-OPOT12-2                  PIC 9 VALUE 0.                         
          02 M-OT122-1                   PIC 9 VALUE 0.                         
          02 M-PCTCUROP-1                PIC 9 VALUE 0.                         
          02 M-PCTCUROP-2                PIC 9 VALUE 0.                         
          02 M-BS050-1                   PIC 9 VALUE 0.                         
          02 M-BS050-2                   PIC 9 VALUE 0.                         
          02 M-BS111-2                   PIC 9 VALUE 0.                         
          02 M-BS111-3                   PIC 9 VALUE 0.                         
          02 M-BS116-1                   PIC 9 VALUE 0.                         
          02 M-CC040-1                   PIC 9 VALUE 0.                         
          02 M-CC040-2                   PIC 9 VALUE 0.                         
          02 M-CC040-3                   PIC 9 VALUE 0.                         
          02 M-CR040-1                   PIC 9 VALUE 0.                         
          02 M-CR040-2                   PIC 9 VALUE 0.                         
          02 M-CR053-1                   PIC 9 VALUE 0.                         
          02 M-CR115-2                   PIC 9 VALUE 0.                         
          02 M-CR116-2                   PIC 9 VALUE 0.                         
          02 M-MT115-1                   PIC 9 VALUE 0.                         
          02 M-MT115-2                   PIC 9 VALUE 0.                         
          02 M-CUREAT-1                  PIC 9 VALUE 0.                         
          02 M-OT002-1                   PIC 9 VALUE 0.                         
          02 M-OT002-2                   PIC 9 VALUE 0.                         
          02 M-OT013-1                   PIC 9 VALUE 0.                         
          02 M-OT014-1                   PIC 9 VALUE 0.                         
          02 M-OT030-1                   PIC 9 VALUE 0.                         
          02 M-OT041-1                   PIC 9 VALUE 0.                         
          02 M-OT119-1                   PIC 9 VALUE 0.                         
          02 M-OT112-1                   PIC 9 VALUE 0.                         
          02 M-OT113-1                   PIC 9 VALUE 0.                         
          02 M-OT121-1                   PIC 9 VALUE 0.                         
          02 M-OT124-1                   PIC 9 VALUE 0.                         
          02 M-CBCR60-1                  PIC 9 VALUE 0.                         
          02 M-CBBS69-1                  PIC 9 VALUE 0.                         
          02 M-PCTCUR-1                  PIC 9 VALUE 0.                         
          02 M-PCTCUR-2                  PIC 9 VALUE 0.                         
          02 M-PCTCUR-3                  PIC 9 VALUE 0.                         
          02 M-PCTCUR-4                  PIC 9 VALUE 0.                         
          02 M-PCTCUR-6                  PIC 9 VALUE 0.                         
          02 M-PCTOP12-1                 PIC 9 VALUE 0.                         
          02 M-PCTOP12-2                 PIC 9 VALUE 0.                         
          02 M-ID005-1                   PIC 9 VALUE 0.                         
       01 VARIABLES-MICROCREDITO.                                               
          02 M-CBCR60                    PIC S9(04) VALUE 0.                    
          02 M-CBBS69                    PIC S9(04) VALUE 0.                    
          02 M-MESESCAN                  PIC S9(04) VALUE 0.                    
          02 M-AOTX                      PIC 9(04) VALUE 0.                     
          02 M-AYTX                      PIC 9(04) VALUE 0.                     
          02 TID005                      PIC 9(03) VALUE 0.                     
          02 M-ANO                       PIC 9(04) VALUE 0.                     
          02 M-IANO                      PIC 9(04) VALUE 0.                     
          02 M-M120OPEOR                 PIC 9(03) VALUE 0.                     
SAL113    02 M-SALARIOMINIMO             PIC 9(08) VALUE 590. *> EN MILEGJC0019
          02 M-LIMBAJBS111               PIC 9(10) VALUE 0.                     
          02 M-LIMMEDBS111               PIC 9(10) VALUE 0.                     
          02 M-LIMALTBS111               PIC 9(10) VALUE 0.                     
          02 M-FA                        PIC 9(05)V9999 VALUE 0.                
          02 M-SALARIOSMINIMOS.                                                 
SMIN10*      03 FILLER                   PIC 9(08) VALUE 19900080.      GJC0015
SMIN11*      03 FILLER                   PIC 9(08) VALUE 19910100.      CAV0016
SMIN12*      03 FILLER                   PIC 9(08) VALUE 19920120.      GJC0018 
SMIN13*      03 FILLER                   PIC 9(08) VALUE 19930140.      GJC0019 
             03 FILLER                   PIC 9(08) VALUE 19940160.              
             03 FILLER                   PIC 9(08) VALUE 19950180.              
             03 FILLER                   PIC 9(08) VALUE 19960200.              
             03 FILLER                   PIC 9(08) VALUE 19970220.              
             03 FILLER                   PIC 9(08) VALUE 19980240.              
             03 FILLER                   PIC 9(08) VALUE 19990260.              
             03 FILLER                   PIC 9(08) VALUE 20000280.              
             03 FILLER                   PIC 9(08) VALUE 20010300.              
             03 FILLER                   PIC 9(08) VALUE 20020320.              
             03 FILLER                   PIC 9(08) VALUE 20030340.              
             03 FILLER                   PIC 9(08) VALUE 20040358. *>OK         
             03 FILLER                   PIC 9(08) VALUE 20050382. *>OK         
             03 FILLER                   PIC 9(08) VALUE 20060408.              
             03 FILLER                   PIC 9(08) VALUE 20070434.              
SMIN08       03 FILLER                   PIC 9(08) VALUE 20080461. *>OK  GJC0010
SMIN09       03 FILLER                   PIC 9(08) VALUE 20090497.       GJC0013
SMIN10       03 FILLER                   PIC 9(08) VALUE 20100515.      GJC0015
SAL112       03 FILLER                   PIC 9(08) VALUE 20110536.      GJC0017
SMIN12       03 FILLER                   PIC 9(08) VALUE 20120567.      GJC0018
SMIN13       03 FILLER                   PIC 9(08) VALUE 20130590.      GJC0019
          02 FILLER REDEFINES M-SALARIOSMINIMOS.                                
             03 FILLER OCCURS 20 TIMES.                                         
                04 M-ANOMINIMO           PIC 9(04).                             
                04 M-SALMINIMO           PIC 9(04).                             
       01 VARIABLES-ACIERTA-PLUS-ZERO.                                          
          02 P-INDEX                     PIC 99V9(6).                           
          02 P-SCOREN                    PIC S999999V9(12).                     
          02 P-SCOREN1                   PIC S999999V9(12).                     
          02 P-COEF1                     PIC S999999V9(12).                     
          02 P-COEF2                     PIC S999999V9(12).                     
          02 P-COEF3                     PIC S999999V9(12).                     
          02 P-COEF4                     PIC S999999V9(12).                     
          02 P-COEF5                     PIC S999999V9(12).                     
          02 P-COEF6                     PIC S999999V9(12).                     
          02 P-MUESTRA                   PIC 9 VALUE 0. *>1=MUESTRA INIC        
          02 P-SCORE-CARD                PIC 9(1).                              
          02 P-SCORE                     PIC 9(03).                             
          02 P-SCORE-REAL                PIC 9(03).                             
          02 P-EXCLUSIONES               PIC 9(01).                             
          02 P-APLICAR-EXCLUSION         PIC 9(01) VALUE 1.                     
          02 VARIABLES-DUMMYS-ACIERTA-PLUS.                                     
             03 VARDUM-ACIPLU-SCOCR1.                                           
                04 P-AOT001            PIC 9.                                   
                04 P-AOT002            PIC 9.                                   
                04 P-AOT003            PIC 9.                                   
                04 P-AH003001          PIC 9.                                   
                04 P-AH003002          PIC 9.                                   
                04 P-AH003003          PIC 9.                                   
                04 P-AH003004          PIC 9.                                   
                04 P-AT124001          PIC 9.                                   
                04 P-AT124002          PIC 9.                                   
                04 P-AT124003          PIC 9.                                   
                04 P-CC002001          PIC 9.                                   
                04 P-CC002002          PIC 9.                                   
                04 P-CC111001          PIC 9.                                   
                04 P-CC111002          PIC 9.                                   
                04 P-CC111003          PIC 9.                                   
                04 P-IQ013001          PIC 9.                                   
                04 P-IQ013002          PIC 9.                                   
                04 P-IQ013003          PIC 9.                                   
                04 P-IQ013004          PIC 9.                                   
                04 P-AYT001            PIC 9.                                   
                04 P-AYT002            PIC 9.                                   
                04 P-ID007001          PIC 9.                                   
                04 P-ID007002          PIC 9.                                   
                04 P-ID007003          PIC 9.                                   
                04 P-ID007004          PIC 9.                                   
                04 P-RECDELP001        PIC 9.                                   
                04 P-RECDELP002        PIC 9.                                   
                04 P-RECDELP003        PIC 9.                                   
                04 P-RECDELP004        PIC 9.                                   
                04 P-RECDELP005        PIC 9.                                   
                04 P-HIBRIDA001        PIC 9.                                   
                04 P-HIBRIDA002        PIC 9.                                   
             03 VARDUM-ACIPLU-SCOCR2.                                           
                04 P-HIBRIDA003        PIC 9.                                   
                04 P-HIBRIDA004        PIC 9.                                   
                04 P-HIBRIDA005        PIC 9.                                   
                04 P-HIBRIDA006        PIC 9.                                   
                04 P-ID007005          PIC 9.                                   
                04 P-ID007006          PIC 9.                                   
                04 P-ID007007          PIC 9.                                   
                04 P-ID007008          PIC 9.                                   
                04 P-ID007009          PIC 9.                                   
                04 P-AOT004            PIC 9.                                   
                04 P-AOT005            PIC 9.                                   
                04 P-AOT006            PIC 9.                                   
                04 P-AOT007            PIC 9.                                   
                04 P-AOT008            PIC 9.                                   
                04 P-AOT009            PIC 9.                                   
                04 P-AOT010            PIC 9.                                   
                04 P-CR115001          PIC 9.                                   
                04 P-CR115002          PIC 9.                                   
                04 P-CR115003          PIC 9.                                   
                04 P-CR115004          PIC 9.                                   
                04 P-CR115005          PIC 9.                                   
                04 P-CR115006          PIC 9.                                   
                04 P-CR115007          PIC 9.                                   
                04 P-CR115008          PIC 9.                                   
                04 P-CR115009          PIC 9.                                   
                04 P-CR115010          PIC 9.                                   
                04 P-AT124004          PIC 9.                                   
                04 P-AT124005          PIC 9.                                   
                04 P-AT124006          PIC 9.                                   
                04 P-AT124007          PIC 9.                                   
                04 P-AT124008          PIC 9.                                   
                04 P-AT124009          PIC 9.                                   
                04 P-TOT30001          PIC 9.                                   
                04 P-TOT30002          PIC 9.                                   
                04 P-TOT60001          PIC 9.                                   
                04 P-TOT60002          PIC 9.                                   
                04 P-RECDEL001         PIC 9.                                   
                04 P-RECDEL002         PIC 9.                                   
                04 P-RECDEL003         PIC 9.                                   
                04 P-RECDEL004         PIC 9.                                   
                04 P-RECDEL005         PIC 9.                                   
                04 P-IQ1-2001          PIC 9.                                   
                04 P-IQ1-2002          PIC 9.                                   
                04 P-IQ1-2003          PIC 9.                                   
                04 P-BSBS001           PIC 9.                                   
                04 P-BSBS002           PIC 9.                                   
                04 P-BSBS003           PIC 9.                                   
                04 P-BSBS004           PIC 9.                                   
                04 P-MTMT001           PIC 9.                                   
                04 P-MTMT002           PIC 9.                                   
                04 P-MTMT003           PIC 9.                                   
                04 P-MTMT004           PIC 9.                                   
                04 P-CC041001          PIC 9.                                   
                04 P-CC041002          PIC 9.                                   
                04 P-CC041003          PIC 9.                                   
                04 P-CC041004          PIC 9.                                   
                04 P-CC041005          PIC 9.                                   
                04 P-CC041006          PIC 9.                                   
                04 P-CC041007          PIC 9.                                   
                04 P-CC041008          PIC 9.                                   
                04 P-CC041009          PIC 9.                                   
                04 P-CC041010          PIC 9.                                   
                04 P-MOSCR30P001       PIC 9.                                   
                04 P-MOSCR30P002       PIC 9.                                   
                04 P-MOSCR30P003       PIC 9.                                   
                04 P-MOSCR30P004       PIC 9.                                   
                04 P-PCT69001          PIC 9.                                   
                04 P-PCT69002          PIC 9.                                   
                04 P-PCT69003          PIC 9.                                   
                04 P-PCT69004          PIC 9.                                   
                04 P-PCT69005          PIC 9.                                   
                04 P-PCT69006          PIC 9.                                   
                04 P-PCT69007          PIC 9.                                   
                04 P-PCT69008          PIC 9.                                   
                04 P-CBAT69007         PIC 9.                                   
                04 P-CBAT69008         PIC 9.                                   
                04 P-CBAT69009         PIC 9.                                   
                04 P-CBAT69010         PIC 9.                                   
                04 P-CBAT120001        PIC 9.                                   
                04 P-CBAT120002        PIC 9.                                   
                04 P-CBAT120003        PIC 9.                                   
                04 P-CBAT120004        PIC 9.                                   
                04 P-HIBRIDA007        PIC 9.                                   
                04 P-HIBRIDA008        PIC 9.                                   
             03 VARDUM-ACIPLU-SCOCR3.                                           
                04 P-AH004001          PIC 9.                                   
                04 P-AH004002          PIC 9.                                   
                04 P-AH004003          PIC 9.                                   
                04 P-CK006001          PIC 9.                                   
                04 P-CK006002          PIC 9.                                   
                04 P-CK006003          PIC 9.                                   
                04 P-CR115011          PIC 9.                                   
                04 P-CR115012          PIC 9.                                   
                04 P-CR115013          PIC 9.                                   
                04 P-CR115014          PIC 9.                                   
                04 P-IQ1-2004          PIC 9.                                   
                04 P-IQ1-2005          PIC 9.                                   
                04 P-IQ1-2006          PIC 9.                                   
                04 P-IQ013005          PIC 9.                                   
                04 P-IQ013006          PIC 9.                                   
                04 P-OT030001          PIC 9.                                   
                04 P-OT030002          PIC 9.                                   
                04 P-OT124001          PIC 9.                                   
                04 P-OT124002          PIC 9.                                   
                04 P-RECDEL006         PIC 9.                                   
                04 P-RECDEL007         PIC 9.                                   
                04 P-RECDEL008         PIC 9.                                   
                04 P-SUM030001         PIC 9.                                   
                04 P-SUM030002         PIC 9.                                   
                04 P-ID007010          PIC 9.                                   
                04 P-ID007011          PIC 9.                                   
                04 P-ID007012          PIC 9.                                   
                04 P-ID007013          PIC 9.                                   
                04 P-ID007014          PIC 9.                                   
                04 P-ID007015          PIC 9.                                   
                04 P-BS122001          PIC 9.                                   
                04 P-BS122002          PIC 9.                                   
             03 VARDUM-ACIPLU-SCOCR4.                                           
                04 P-AT115001          PIC 9.                                   
                04 P-AT115002          PIC 9.                                   
                04 P-AT115003          PIC 9.                                   
                04 P-AT115004          PIC 9.                                   
                04 P-AT115005          PIC 9.                                   
                04 P-AT115006          PIC 9.                                   
                04 P-AT115007          PIC 9.                                   
                04 P-AT115008          PIC 9.                                   
                04 P-BS122003          PIC 9.                                   
                04 P-BS122004          PIC 9.                                   
                04 P-BS122005          PIC 9.                                   
                04 P-CC032001          PIC 9.                                   
                04 P-CC032002          PIC 9.                                   
                04 P-CC032003          PIC 9.                                   
                04 P-CP103001          PIC 9.                                   
                04 P-CP103002          PIC 9.                                   
                04 P-CP103003          PIC 9.                                   
                04 P-CR116001          PIC 9.                                   
                04 P-CR116002          PIC 9.                                   
                04 P-CR116003          PIC 9.                                   
                04 P-CR116004          PIC 9.                                   
                04 P-CR116005          PIC 9.                                   
                04 P-CR116006          PIC 9.                                   
                04 P-CR116007          PIC 9.                                   
                04 P-CR116008          PIC 9.                                   
                04 P-CR116009          PIC 9.                                   
                04 P-IQ001001          PIC 9.                                   
                04 P-IQ001002          PIC 9.                                   
                04 P-IQ013007          PIC 9.                                   
                04 P-IQ013008          PIC 9.                                   
                04 P-PCTOP12001        PIC 9.                                   
                04 P-PCTOP12002        PIC 9.                                   
                04 P-PCTOP12003        PIC 9.                                   
                04 P-CBAT69005         PIC 9.                                   
                04 P-CBAT69006         PIC 9.                                   
                04 P-ID007016          PIC 9.                                   
                04 P-ID007017          PIC 9.                                   
                04 P-ID007018          PIC 9.                                   
                04 P-ID007019          PIC 9.                                   
                04 P-ID007020          PIC 9.                                   
                04 P-OPCC12001         PIC 9.                                   
                04 P-OPCC12002         PIC 9.                                   
                04 P-OPCC12003         PIC 9.                                   
                04 P-MT040001          PIC 9.                                   
                04 P-MT040002          PIC 9.                                   
                04 P-RECDEL009         PIC 9.                                   
                04 P-RECDEL010         PIC 9.                                   
                04 P-RECDEL011         PIC 9.                                   
                04 P-RECDEL012         PIC 9.                                   
                04 P-RECDEL013         PIC 9.                                   
                04 P-RECDEL014         PIC 9.                                   
                04 P-OT041001          PIC 9.                                   
                04 P-OT041002          PIC 9.                                   
                04 P-CC100001          PIC 9.                                   
                04 P-CC100002          PIC 9.                                   
                04 P-PCTCUR001         PIC 9.                                   
                04 P-PCTCUR002         PIC 9.                                   
             03 VARDUM-ACIPLU-SCOCR5.                                           
                04 P-AT124010          PIC 9.                                   
                04 P-AT124011          PIC 9.                                   
                04 P-AT124012          PIC 9.                                   
                04 P-AT124013          PIC 9.                                   
                04 P-AT124014          PIC 9.                                   
                04 P-AT124015          PIC 9.                                   
                04 P-AT124016          PIC 9.                                   
                04 P-AT124017          PIC 9.                                   
                04 P-CBCC60001         PIC 9.                                   
                04 P-CBCC60002         PIC 9.                                   
                04 P-CBCC60003         PIC 9.                                   
                04 P-CC041011          PIC 9.                                   
                04 P-CC041012          PIC 9.                                   
                04 P-CC041013          PIC 9.                                   
                04 P-CC041014          PIC 9.                                   
                04 P-CC041015          PIC 9.                                   
                04 P-CC041016          PIC 9.                                   
                04 P-CC041017          PIC 9.                                   
                04 P-CC041018          PIC 9.                                   
                04 P-CC041019          PIC 9.                                   
                04 P-CR034001          PIC 9.                                   
                04 P-CR034002          PIC 9.                                   
                04 P-CR034003          PIC 9.                                   
                04 P-CR034004          PIC 9.                                   
                04 P-CR116010          PIC 9.                                   
                04 P-CR116011          PIC 9.                                   
                04 P-CR116012          PIC 9.                                   
                04 P-CR116013          PIC 9.                                   
                04 P-CR116014          PIC 9.                                   
                04 P-CR116015          PIC 9.                                   
                04 P-CR116016          PIC 9.                                   
                04 P-CR116017          PIC 9.                                   
                04 P-CR116018          PIC 9.                                   
                04 P-CR116019          PIC 9.                                   
                04 P-CR116020          PIC 9.                                   
                04 P-CR116021          PIC 9.                                   
                04 P-CR116022          PIC 9.                                   
                04 P-CR116023          PIC 9.                                   
                04 P-CR116024          PIC 9.                                   
                04 P-CR116025          PIC 9.                                   
                04 P-ID004001          PIC 9.                                   
                04 P-ID004002          PIC 9.                                   
                04 P-ID004003          PIC 9.                                   
                04 P-ID004004          PIC 9.                                   
                04 P-ID004005          PIC 9.                                   
                04 P-ID004006          PIC 9.                                   
                04 P-ID004007          PIC 9.                                   
                04 P-ID004008          PIC 9.                                   
                04 P-ID004009          PIC 9.                                   
                04 P-ID004010          PIC 9.                                   
                04 P-ID004011          PIC 9.                                   
                04 P-ID004012          PIC 9.                                   
                04 P-ID007021          PIC 9.                                   
                04 P-ID007022          PIC 9.                                   
                04 P-ID007023          PIC 9.                                   
                04 P-ID007024          PIC 9.                                   
                04 P-ID007025          PIC 9.                                   
                04 P-IQ013009          PIC 9.                                   
                04 P-IQ013010          PIC 9.                                   
                04 P-IQ013011          PIC 9.                                   
                04 P-IQ013012          PIC 9.                                   
                04 P-IQ013013          PIC 9.                                   
                04 P-PCTOP12004        PIC 9.                                   
                04 P-PCTOP12005        PIC 9.                                   
                04 P-PCTOP12006        PIC 9.                                   
                04 P-PCTOP12007        PIC 9.                                   
                04 P-PCTOP12008        PIC 9.                                   
                04 P-PCTOP12009        PIC 9.                                   
                04 P-PCTOP12010        PIC 9.                                   
                04 P-PCTOP12011        PIC 9.                                   
                04 P-RECDEL015         PIC 9.                                   
                04 P-RECDEL016         PIC 9.                                   
                04 P-RECDEL017         PIC 9.                                   
                04 P-RECDEL018         PIC 9.                                   
                04 P-RECDEL019         PIC 9.                                   
                04 P-RECDEL020         PIC 9.                                   
                04 P-RECDEL021         PIC 9.                                   
             03 VARDUM-ACIPLU-SCOCR6.                                           
                04 P-AT114001          PIC 9.                                   
                04 P-AT114002          PIC 9.                                   
                04 P-AT114003          PIC 9.                                   
                04 P-AT114004          PIC 9.                                   
                04 P-AT114005          PIC 9.                                   
                04 P-AT114006          PIC 9.                                   
                04 P-AT114007          PIC 9.                                   
                04 P-AT116001          PIC 9.                                   
                04 P-AT116002          PIC 9.                                   
                04 P-AT116003          PIC 9.                                   
                04 P-AT116004          PIC 9.                                   
                04 P-AT116005          PIC 9.                                   
                04 P-AT116006          PIC 9.                                   
                04 P-AT116007          PIC 9.                                   
                04 P-AT116008          PIC 9.                                   
                04 P-AT116009          PIC 9.                                   
                04 P-AT116010          PIC 9.                                   
                04 P-AT116011          PIC 9.                                   
                04 P-AT116012          PIC 9.                                   
                04 P-AT124018          PIC 9.                                   
                04 P-AT124019          PIC 9.                                   
                04 P-AT124020          PIC 9.                                   
                04 P-AT124021          PIC 9.                                   
                04 P-AT124022          PIC 9.                                   
                04 P-BS050001          PIC 9.                                   
                04 P-BS050002          PIC 9.                                   
                04 P-BS050003          PIC 9.                                   
                04 P-BS115001          PIC 9.                                   
                04 P-BS115002          PIC 9.                                   
                04 P-BS115003          PIC 9.                                   
                04 P-BS115004          PIC 9.                                   
                04 P-BS115005          PIC 9.                                   
                04 P-BS115006          PIC 9.                                   
                04 P-BS115007          PIC 9.                                   
                04 P-BS115008          PIC 9.                                   
                04 P-BS115009          PIC 9.                                   
                04 P-CR050001          PIC 9.                                   
                04 P-CR050002          PIC 9.                                   
                04 P-CR050003          PIC 9.                                   
                04 P-ID004013          PIC 9.                                   
                04 P-ID004014          PIC 9.                                   
                04 P-ID004015          PIC 9.                                   
                04 P-ID004016          PIC 9.                                   
                04 P-ID004017          PIC 9.                                   
                04 P-ID004018          PIC 9.                                   
                04 P-ID004019          PIC 9.                                   
                04 P-ID004020          PIC 9.                                   
                04 P-ID004021          PIC 9.                                   
                04 P-ID004022          PIC 9.                                   
                04 P-ID004023          PIC 9.                                   
                04 P-ID004024          PIC 9.                                   
                04 P-ID004025          PIC 9.                                   
                04 P-ID004026          PIC 9.                                   
                04 P-ID004027          PIC 9.                                   
                04 P-ID004028          PIC 9.                                   
                04 P-ID004029          PIC 9.                                   
                04 P-ID004030          PIC 9.                                   
                04 P-ID004031          PIC 9.                                   
                04 P-ID004032          PIC 9.                                   
                04 P-ID004033          PIC 9.                                   
                04 P-ID004034          PIC 9.                                   
                04 P-ID004035          PIC 9.                                   
                04 P-IQ013014          PIC 9.                                   
                04 P-IQ013015          PIC 9.                                   
                04 P-IQ013016          PIC 9.                                   
                04 P-AOT011            PIC 9.                                   
                04 P-AOT012            PIC 9.                                   
                04 P-AOT013            PIC 9.                                   
                04 P-AOT014            PIC 9.                                   
                04 P-AOT015            PIC 9.                                   
                04 P-AOT016            PIC 9.                                   
                04 P-AOT017            PIC 9.                                   
                04 P-AOT018            PIC 9.                                   
                04 P-AOT019            PIC 9.                                   
                04 P-AOT020            PIC 9.                                   
                04 P-AOT021            PIC 9.                                   
                04 P-AOT022            PIC 9.                                   
                04 P-MOS60001          PIC 9.                                   
                04 P-MOS60002          PIC 9.                                   
                04 P-MOS60003          PIC 9.                                   
                04 P-OPOT18001         PIC 9.                                   
                04 P-OPOT18002         PIC 9.                                   
                04 P-OPOT18003         PIC 9.                                   
                04 P-TOT30P001         PIC 9.                                   
                04 P-TOT30P002         PIC 9.                                   
                04 P-ID007026          PIC 9.                                   
                04 P-ID007027          PIC 9.                                   
                04 P-ID007028          PIC 9.                                   
                04 P-ID007029          PIC 9.                                   
                04 P-ID007030          PIC 9.                                   
             03 VARDUM-ACIPLU-SCOCR6.                                           
                04 P-AT114008          PIC 9.                                   
                04 P-AT114009          PIC 9.                                   
                04 P-AT114010          PIC 9.                                   
                04 P-AT114011          PIC 9.                                   
                04 P-AT114012          PIC 9.                                   
                04 P-AT114013          PIC 9.                                   
                04 P-AT114014          PIC 9.                                   
                04 P-AT114015          PIC 9.                                   
                04 P-AT124023          PIC 9.                                   
                04 P-AT124024          PIC 9.                                   
                04 P-AT124025          PIC 9.                                   
                04 P-AT124026          PIC 9.                                   
                04 P-AT124027          PIC 9.                                   
                04 P-AT124028          PIC 9.                                   
                04 P-AT124029          PIC 9.                                   
                04 P-AT124030          PIC 9.                                   
                04 P-CC040001          PIC 9.                                   
                04 P-CC040002          PIC 9.                                   
                04 P-CC040003          PIC 9.                                   
                04 P-CC040004          PIC 9.                                   
                04 P-CC040005          PIC 9.                                   
                04 P-CR040001          PIC 9.                                   
                04 P-CR040002          PIC 9.                                   
                04 P-CR040003          PIC 9.                                   
                04 P-CR040004          PIC 9.                                   
                04 P-CR040005          PIC 9.                                   
                04 P-CR116026          PIC 9.                                   
                04 P-CR116027          PIC 9.                                   
                04 P-CR116028          PIC 9.                                   
                04 P-CR116029          PIC 9.                                   
                04 P-CR116030          PIC 9.                                   
                04 P-CR116031          PIC 9.                                   
                04 P-CR116032          PIC 9.                                   
                04 P-CR116033          PIC 9.                                   
                04 P-CR116034          PIC 9.                                   
                04 P-CR116035          PIC 9.                                   
                04 P-IQ013017          PIC 9.                                   
                04 P-IQ013018          PIC 9.                                   
                04 P-IQ013019          PIC 9.                                   
                04 P-PCTOP12012        PIC 9.                                   
                04 P-PCTOP12013        PIC 9.                                   
                04 P-PCTOP12014        PIC 9.                                   
                04 P-RECDEL022         PIC 9.                                   
                04 P-RECDEL023         PIC 9.                                   
                04 P-RECDEL024         PIC 9.                                   
                04 P-RECDEL025         PIC 9.                                   
                04 P-RECDEL026         PIC 9.                                   
                04 P-RECDEL027         PIC 9.                                   
                04 P-RECDEL028         PIC 9.                                   
                04 P-RECDEL029         PIC 9.                                   
                04 P-CR050004          PIC 9.                                   
                04 P-CR050005          PIC 9.                                   
                04 P-CR050006          PIC 9.                                   
                04 P-CR050007          PIC 9.                                   
                04 P-CCCC001           PIC 9.                                   
                04 P-CCCC002           PIC 9.                                   
                04 P-CCCC003           PIC 9.                                   
                04 P-CCCC004           PIC 9.                                   
                04 P-BSBS005           PIC 9.                                   
                04 P-BSBS006           PIC 9.                                   
                04 P-BSBS007           PIC 9.                                   
                04 P-ID007031          PIC 9.                                   
                04 P-ID007032          PIC 9.                                   
                04 P-ID007033          PIC 9.                                   
                04 P-ID007034          PIC 9.                                   
          02 VARIABLES-ACIERTA-PLUS-HIBR.                                       
             03 P-SUM001            PIC S9(005).                                
             03 P-SUM002            PIC S9(005).                                
             03 P-AT104             PIC S9(005).                                
             03 P-SOLOCELU          PIC S9(005).                                
             03 P-DERAT             PIC S9(005).                                
             03 P-TOT30             PIC S9(005).                                
             03 P-TOT60             PIC S9(005).                                
             03 P-ID004F            PIC S9(005).                                
             03 P-TOT90P            PIC S9(005).                                
             03 P-TOT30P            PIC S9(005).                                
             03 P-TOT90             PIC S9(005).                                
             03 P-TOT120            PIC S9(005).                                
             03 P-IQ1-2             PIC S9(005).                                
             03 P-BSBS              PIC S9(005).                                
             03 P-CCCC              PIC S9(005).                                
             03 P-MTMT              PIC S9(005).                                
             03 P-MOSCC             PIC S9(005).                                
             03 P-SUM030            PIC S9(005).                                
             03 P-ID004X            PIC S9(005).                                
             03 P-CBCC60            PIC S9(005).                                
             03 P-MOS60             PIC S9(005).                                
             03 P-OPOT18            PIC S9(005).                                
             03 P-CBAT90            PIC S9(005).                                
             03 P-CBAT90P           PIC S9(005).                                
             03 P-CBAT120           PIC S9(005).                                
ACIEM9       03 P-CBAT120P          PIC S9(005).                        RMB0011
             03 P-PCTCUR            PIC S9(005).                                
             03 P-AOT               PIC S9(005).                                
             03 P-MOSCR30P          PIC S9(005).                                
             03 P-RECDEL            PIC S9(005).                                
             03 P-RECDELP           PIC S9(005).                                
             03 P-AYT               PIC S9(005).                                
             03 P-OPCC12            PIC S9(005).                                
             03 P-CBAT69            PIC S9(005).                                
             03 P-PCT69             PIC S9(005).                                
             03 P-PCTOP12           PIC S9(005).                                
             03 P-DEFOT90P          PIC S9(005).                                
             03 P-DEFOT             PIC S9(005).                                
