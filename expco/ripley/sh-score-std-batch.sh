/d/iccol/desarrollo/macros>cat sh-score-std-batch
#sh-score-std-batch.v01.0009.
#**********************************************************
#SHELL-ID:     sh-score-std-batch
#DATE-WRITTEN: NOV/2008
#AUTHOR:       DIRECCION DE PROCESOS ESPECIALES           *
#PROGRAM-ID:   SCOTBATCH.CBL
#*========================================================*
#AGO-17-2010| REQ509 - Para QUANTO se pide formato 1-11   *
#LAG0003    |          (No valida) o 1-11-45 (Si valida)  *
#ger.desarr.|        - Mostrar nombre de archivo de salida*
#*========================================================*
#DIC-22-2010| REQ962 - Para QUANTO se pide formato:       *
#LAG0004    |        - 1 - 11 Busca nombres y valida      *
#ger.desarr.|        - 1 - 11 - 45 valida                 *
#==============================================================================|
# PJV0004    | CQ5317 - SE AJUSTA MACRO PARA ENRUTAMIENTO DE ERROR DE EJECUCIÓN|
# 03-OCT-2012|          a (2>>)                                                |
# PEDRO V.  .| ALCANCE PARA AJUSTAR CONTROL DE SANATA A 2 DIGITOS              |
#==============================================================================|
# PJV0005    | CQ6455 - SE INCLUYE CALCULO DE ACIERTA A DE ACUERDO CON LA      |
# 30-OCT-2012| SIGUIENTE TABLA                                                 |
# PEDRO V.   |  +--+----------------------------------------+                  |
#            |  |CG| DESCRIPCION DEL SCORE                  |                  |
#            |  +--+----------------------------------------+                  |
#            |  |41|ACIERTA A VEHÍCULO E HIPOTECARIO LÍNEA  |                  |
#            |  |45|ACIERTA A COOPERATIVAS LÍNEA            |                  |
#            |  |47|ACIERTA A FINANCIERO LÍNEA              |                  |
#            |  |48|ACIERTA A TARJETA DE CRÉDITO LÍNEA      |                  |
#            |  |49|ACIERTA A TELECOMUNICACIONES LÍNEA      |                  |
#            |  |95|ACIERTA A INSTALAMENTOS LÍNEA           |                  |
#            |  |99|CARACTERISTICAS                         |                  |
#            |  +--+----------------------------------------+                  |
#==============================================================================|
# PJV0006    | CQ7550 - AJUSTES NUEVAS OPCIONES SCOTBARCH PARA EL              |
# 08-NOV-2012| MANEJO DE COMBOS DE LA SERIE DE SCORES (+)                      |
# PEDRO V.   |                                                                 |
#==============================================================================|
# PJV0007    | CQ9226 - QUE CUANDO EL ARCHIVO DE ENTRADA SEA 1-11-45 VALIDE    |
# 04-DIC-2012| Y NO INCLUYA EL REGISTRO VALIDAR=NO                             |
# PEDRO V.   |                                                                 |
#==============================================================================|
# STT0008    | R15689 - INCLUSION DE PRODUCTOS MICROCREDITO (119 Y 120)        |
# 22-OCT-2013| CQ-15689                                                        |
# LUIS GOMEZ |                                                                 |
#==============================================================================|
# DLM0009    | QXXXXX - OPCION STABILITY.                                      |
# 26-MAY-2014| CQ-XXXXX                                                        |
# DARIO LASSO|                                                                 |
#==============================================================================|

GetFechaSistema()
{
#*-----------------------------------------------------------------------------*
# ACTUALIZA FECHA ACTUAL DEL SISTEMA
#*-----------------------------------------------------------------------------*
    P_SERVER_DEV="172.24.6.154"
    HoraHoy=`date '+%H:%M'`
    FECHOY_A4MMDD=`date '+%Y%m%d'`
    FECHOY_ANOMES=`date '+%Y%m%d' | cut -c 1-6`
    P_FCORTE=$FECHOY_ANOMES
    P_MAQUINA=`who am i | cut -c 39-50`
    echo "MAQUINA: " $P_MAQUINA
    echo "+-------------------------------------------------------------+"
    echo "|     Fecha de Ejecucion    :      " $FECHOY_A4MMDD
    echo "|     Hora de Ejecucion     :      " $HoraHoy
    echo "|-------------------------------------------------------------+"
}

GetEnter()
{
#*-----------------------------------------------------------------------------*
# SOLICITA OPRIMIR ENTER PARA CONTINUAR EJECUCION
#*-----------------------------------------------------------------------------*
     echo
     echo "            (ENTER) Continuar !!!  "
     read TECLA
}


MuestraSelect()
{
        echo "+-------------------------------------------------------+"
        echo "|      **  Opciones Scoring Seleccionados          **   |"
        echo "+-------------------------------------------------------+"
        echo " [41] ACIERTA A - VEHÍCULO E HIPOTECARIO  : " $ACI41
        echo " [45] ACIERTA A - COOPERATIVAS            : " $ACI45
        echo " [47] ACIERTA A - FINANCIERO              : " $ACI47
        echo " [48] ACIERTA A - TARJETA DE CRÉDITO      : " $ACI48
        echo " [49] ACIERTA A - TELECOMUNICACIONES      : " $ACI49
        echo " [62] QUANTO                              : " $ACI62
        echo " [67] ACIERTA+                            : " $ACI67
        echo " [95] ACIERTA A INSTALAMENTOS             : " $ACI95
        echo " [99] CARACTERISTICAS                     : " $ACI99
        echo "+-------------------------------------------------------+"
}

MuestraSelectM()
{
        echo "+-------------------------------------------------------+"
        echo "|      **  Opciones Scoring Seleccionados          **   |"
        echo "+-------------------------------------------------------+"
        echo " [99 ] CARACTERISTICAS                     : " $ACI99
        echo " [119] MICROCREDITO - BURO                 : " $ACI119
        echo " [120] MICROCREDITO - INF. EXTERNA         : " $ACI120
        echo "+-------------------------------------------------------+"
}

RevisaNit()
{
#CQ6455
if [ $scoring -eq 41 ] || [ $scoring -eq 45 ] ||
   [ $scoring -eq 47 ] || [ $scoring -eq 48 ] ||
   [ $scoring -eq 49 ] || [ $scoring -eq 95 ] ||
   [ $scoring -eq 99 ] || [ $scoring -eq 119 ] ||
   [ $scoring -eq 120 ] || [ $opcion -eq 101 ] || [ $opcion -eq 103 ];
   then
  echo "Digite el NIT del suscriptor (9 Digitos ) -----> \c"
  read nitsus
fi
}


ViewSelectCombos()
#*-----------------------------------------------------------------------------*
# Muestr los SCORES seleccionados para enviar a ejecucion
# ESTA FUNCION MULTIPLE SOLO APLICA PARA LA SERIE ACIERTA A (+)
#*-----------------------------------------------------------------------------*
{
        MuestraSelect
        echo "+-------------------------------------------------------+"
        echo "          *  <M>   Modifica Lista                        "
        echo "             <T>   Terminar                              "
        echo "*-------------------------------------------------------*"
        echo "       Digite opcion  (M/T)   ---> \c"
        read P_OPCPRO
        if [ $P_OPCPRO == "M" ] || [ $P_OPCPRO == "m" ] ; then
            P_OPCPRO="M"
            UpdateListProduc
#           SaveParametros
        else
            if [ $P_OPCPRO == "T" ] || [ $P_OPCPRO == "t" ] ; then
                  P_OPCPRO="T"
            else
                echo "\n\t*ERRROR* OPCION NO VALIDA! <ENTER>\c"
                GetEnter
            fi
        fi
}


UpdateListProduc()
{
P_ALL_SCORE="S";
echo "+------------------------------------------------------+"
echo "|    *** COMBOS DE SCORES  ***            |"
echo "+------------------------------------------------------+"
echo "Todos los SCORES    S/N ? : \c"
read P_ALL_SCORE
if test -z "$P_ALL_SCORE" ; then
   P_ALL_SCORE="S"
fi
if [ $P_ALL_SCORE -eq "S" ] ; then
   ACI41="S"
   ACI45="S"
   ACI47="S"
   ACI48="S"
   ACI49="S"
   ACI62="S"
   ACI67="S"
   ACI95="S"
   ACI99="S"
else
   echo "[41] ACIERTA A - VEHÍCULO E HIPOTECARIO : \c"
   read ACI41
   if test -z "$ACI41"
    then
     ACI41="S"
   fi

   echo "[45] ACIERTA A - COOPERATIVAS           : \c"
   read ACI45
   if test -z "$ACI45"
    then
     ACI45="S"
   fi

   echo "[47] ACIERTA A - FINANCIERO             : \c"
   read ACI47
   if test -z "$ACI47"
    then
     ACI47="S"
   fi

   echo "[48] ACIERTA A - TARJETA DE CRÉDITO     : \c"
   read ACI48
   if test -z "$ACI48"
    then
     ACI48="S"
   fi

   echo "[49] ACIERTA A - TELECOMUNICACIONES     : \c"
   read ACI49
   if test -z "$ACI49"
    then
     ACI49="S"
   fi

   echo "[62] QUANTO                             : \c"
   read ACI62
   if test -z "$ACI62"
    then
     ACI62="S"
   fi

   echo "[67] ACIERTA+                           : \c"
   read ACI67
   if test -z "$ACI67"
    then
     ACI67="S"
   fi

   echo "[95] ACIERTA A - INSTALAMENTOS          : \c"
   read ACI95
   if test -z "$ACI95"
    then
     ACI95="S"
   fi

   echo "[99] CARACTERISTICAS                    : \c"
   read ACI99
   if test -z "$ACI99"
    then
     ACI99="S"
   fi
fi
MuestraSelect
GetEnter
}


ViewSelectCombosM()
#*-----------------------------------------------------------------------------*
# Muestra los SCORES seleccionados para enviar a ejecucion
# ESTA FUNCION MULTIPLE SOLO APLICA PARA LA SERIE ACIERTA A (+) MICROCR.
#*-----------------------------------------------------------------------------*
{
        MuestraSelectM
        echo "+-------------------------------------------------------+"
        echo "          *  <M>   Modifica Lista                        "
        echo "             <T>   Terminar                              "
        echo "*-------------------------------------------------------*"
        echo "       Digite opcion  (M/T)   ---> \c"
        read P_OPCPRO
        if [ $P_OPCPRO == "M" ] || [ $P_OPCPRO == "m" ] ; then
            P_OPCPRO="M"
            UpdateListProducM
#           SaveParametros
        else
            if [ $P_OPCPRO == "T" ] || [ $P_OPCPRO == "t" ] ; then
                  P_OPCPRO="T"
            else
                echo "\n\t*ERRROR* OPCION NO VALIDA! <ENTER>\c"
                GetEnter
            fi
        fi
}


UpdateListProducM()
{

ACI99="S"
if [ $ACI119 == "S"  ] ; then
  ACI119="N"
  ACI120="S"
else
  ACI119="S"
  ACI120="N"
fi
MuestraSelectM
GetEnter
}


ExecuteSCOTBATCHcombo()
{
echo "+--------------------------------------------+"
echo "|  Ejecucion SCOTBATCH en COMBO   P_ALL_SCORE:" $P_ALL_SCORE
echo "+--------------------------------------------+"
if [ $P_ALL_SCORE == "S" ] ; then
   echo "CALCULANDO TODOS LOS SCORES SERIE + "
   SCO41="041"
   SCO45="045"
   SCO47="047"
   SCO48="048"
   SCO49="049"
   SCO62="062"
   SCO67="067"
   SCO95="095"
   SCO99="099"
### [[[  DLM - 2014-05-26
   if  [ $OPC_STABILITY == "S" ] ; then
     SCO41="S41"
     SCO45="S45"
     SCO47="S47"
     SCO48="S48"
     SCO49="S49"
     SCO62="S62"
     SCO67="S67"
     SCO95="S95"
     SCO99="S99"
   fi
### ]]]  DLM - 2014-05-26
else
   if [ $ACI41 == "S" ] || [ $ACI41 == "s" ];
    then
      SCO41="041"
### [[[  DLM - 2014-05-26
      if  [ $OPC_STABILITY == "S" ] ; then
         SCO41="S41"
      fi
### ]]]  DLM - 2014-05-26
   else
      SCO41="000"
   fi

   if [ $ACI45 == "S" ] || [ $ACI45 == "s" ];
    then
      SCO45="045"
### [[[  DLM - 2014-05-26
      if  [ $OPC_STABILITY == "S" ] ; then
      SCO45="S45"
      fi
### ]]]  DLM - 2014-05-26

   else
      SCO45="000"
   fi

   if [ $ACI47 == "S" ] || [ $ACI47 == "s" ];
    then
      SCO47="047"
### [[[  DLM - 2014-05-26
      if  [ $OPC_STABILITY == "S" ] ; then
        SCO47="S47"
      fi
### ]]]  DLM - 2014-05-26
   else
      SCO47="000"
   fi

   if [ $ACI48 == "S" ] || [ $ACI48 == "s" ];
    then
      SCO48="048"
### [[[  DLM - 2014-05-26
      if  [ $OPC_STABILITY == "S" ] ; then
        SCO48="S48"
      fi
### ]]]  DLM - 2014-05-26
   else
      SCO48="000"
   fi

   if [ $ACI49 == "S" ] || [ $ACI49 == "s" ];
    then
      SCO49="049"
### [[[  DLM - 2014-05-26
      if  [ $OPC_STABILITY == "S" ] ; then
        SCO49="S49"
      fi
### ]]]  DLM - 2014-05-26
   else
      SCO49="000"
   fi

   if [ $ACI62 == "S" ] || [ $ACI62 == "s" ];
    then
      SCO62="062"
### [[[  DLM - 2014-05-26
      if  [ $OPC_STABILITY == "S" ] ; then
        SCO62="S62"
      fi
### ]]]  DLM - 2014-05-26
    else
      SCO62="000"
   fi

   if [ $ACI67 == "S" ] || [ $ACI67 == "s" ];
    then
      SCO67="067"
### [[[  DLM - 2014-05-26
      if  [ $OPC_STABILITY == "S" ] ; then
        SCO67="S67"
      fi
### ]]]  DLM - 2014-05-26
    else
      SCO67="000"
   fi

   if [ $ACI95 == "S" ] || [ $ACI95 == "s" ];
    then
      SCO95="095"
### [[[  DLM - 2014-05-26
      if  [ $OPC_STABILITY == "S" ] ; then
        SCO95="S95"
      fi
### ]]]  DLM - 2014-05-26
   else
      SCO95="000"
   fi

   if [ $ACI99 == "S" ] || [ $ACI99 == "s" ];
    then
      SCO99="099"
### [[[  DLM - 2014-05-26
      if  [ $OPC_STABILITY == "S" ] ; then
        SCO99="S99"
      fi
### ]]]  DLM - 2014-05-26
   else
      SCO99="000"
   fi
fi
MuestraSelect
nohup x SCOTBATCH $fechabase $SCO41$SCO45$SCO47$SCO48$SCO49$SCO62$SCO67$SCO95$SCO99 $formato $cadena04 $mnto $nitsus >> $cadena06 2>> $cadena06
tail -22 $cadena06
echo "FINALIZA EJECUCION COMBO SCOTBATCH "
}


ExecuteSCOTBATCHcomboM()
{
echo "+--------------------------------------------+"
echo "|  Ejecucion SCOTBATCH en COMBO MICROCREDITO :"
echo "+--------------------------------------------+"
echo "CALCULANDO TODOS LOS SCORES SERIE + MICROCREDITO "
if [ $ACI119 == "S" ] || [ $ACI119 == "s" ];
 then
   SCO99="099"
   SCOMM="119"
else
   SCO99="099"
   SCOMM="120"
fi
SCORELLM="000000000000000000000"
MuestraSelectM
nohup x SCOTBATCH $fechabase $SCO99$SCOMM$SCORELLM $formato $cadena04 $mnto $nitsus >> $cadena06 2>> $cadena06
tail -22 $cadena06
echo "FINALIZA EJECUCION COMBO SCOTBATCH MICROCREDITO "
}



ViewSelectMantenimiento()
#*-----------------------------------------------------------------------------*
# Muestr los SCORES seleccionados para enviar a ejecucion
# ESTA FUNCION MULTIPLE SOLO APLICA PARA LA SERIE ACIERTA A (+)
#*-----------------------------------------------------------------------------*
{
        echo "+-------------------------------------------------------+"
        echo "|      **  MANTENIMIENTO DE SCORES                 **   |"
        echo "+-------------------------------------------------------+"
        echo " [41M] ACIERTA A VEHÍCULO E HIPOTECARIO   "
        echo " [45M] ACIERTA A COOPERATIVAS             "
        echo " [47M] ACIERTA A FINANCIERO               "
        echo " [48M] ACIERTA A TARJETA DE CRÉDITO       "
        echo " [49M] ACIERTA A TELECOMUNICACIONES       "
        echo " [62M] QUANTO                             "
        echo " [67M] ACIERTA+                           "
        echo " [95M] ACIERTA A INSTALAMENTOS            "
        echo " [119M] MICROCREDITO - BURO               "
        echo " [120M] MICROCREDITO - INF. EXTERNA       "
        echo "+-------------------------------------------------------+"
echo "   Digite el tipo Score -->  \c"
read opcion

}


#Bof
#*-----------------------------------------------------------------------------*
#                   MENU PRINCIPAL DE EJECUCION
#*-----------------------------------------------------------------------------*
#
#*========================================================*
#*                    DEFINICION DE VARIABLES             *
#*========================================================*
opcion=0;
fechabase=0;
scoring=0;
formato={};
numero04=0;
cadena01={};
cadena011={};
cadena02={};
cadena03={};
cadena04={};
cadena06={};
cadena07={};
ACI41="S";
ACI45="S";
ACI47="S";
ACI48="S";
ACI49="S";
ACI62="S";
ACI67="S";
ACI95="S";
ACI99="S";
#
#REQ CQ-15689
ACI119="S";
ACI120="N";
#REQ CQ-15689
ACITOT="S";
P_ALL_SCORE="S";
GetFechaSistema

#*========================================================*
#*                    PROCESO                             *
#*========================================================*
cls
echo "+-------------------------------------------------------+"
echo "|      **           MENU PRINCIPAL                 **   |"
echo "+-------------------------------------------------------+"
echo "|      **  Opciones Scoring Batch por validacion   **   |"
echo "+-------------------------------------------------------+"
echo "            [1  ]  Predicta                              "
echo "            [4  ]  Acierta                               "
echo "            [6  ]  Basico                                "
echo "            [8  ]  Acierta - M                           "
echo "+-------------------------------------------------------+"
echo "            [41 ]  ACIERTA A VEHÍCULO E HIPOTECARIO      "
echo "            [45 ]  ACIERTA A COOPERATIVAS                "
echo "            [47 ]  ACIERTA A FINANCIERO                  "
echo "            [48 ]  ACIERTA A TARJETA DE CRÉDITO          "
echo "            [49 ]  ACIERTA A TELECOMUNICACIONES          "
echo "            [62 ]  QUANTO                                "
echo "            [67 ]  ACIERTA+                              "
echo "            [95 ]  ACIERTA A INSTALAMENTOS               "
echo "            [99 ]  CARACTERISTICAS                       "
echo "            [119]  MICROCREDITO - BURO                   "
echo "            [120]  MICROCREDITO - INF. EXT.              "
echo "+-------------------------------------------------------+"
echo "            [101]  EJECUCION MULTIPLE DE SCORES SERIE +  "
echo "            [102]  MANTENIMIENTO DE SCORES               "
echo "            [103]  EJECUCION MULTIPLE DE SCORES MIC      "
echo "            [115]  Retornar menu principal               "
echo "            [116]  Terminar                              "
echo "*------------------------------------------------------*"
echo "   Digite el tipo Score -->  \c"
read opcion
if [ $opcion -eq 102 ] ; then
   ViewSelectMantenimiento
fi
echo
echo "   Fecha  del SCORE  AAAAMM -->  \c"
read fechabase
if test -z "$fechabase" ; then
   fechabase=$P_FCORTE
   echo "fechabase: " $fechabase
fi
echo
echo "   El archivo *.prn debe exixtir en $TEMPORALES para poder procesar. "
echo "   Digite Nombre del archivo sin extension  -->  \c"
read archivo
cadena01=$archivo.prn
cadena011=$archivo.COP
cadena02=$archivo.val
cadena03=$archivo.inc
cadena04=$archivo.ok
cadena06=$archivo.log
echo
#
mnto=""
#
case $opcion in
     1) scoring="001"
        swsco=0
        fmtoi=2
        cadena07=$archivo.PREDICTA;;

     4) scoring="004"
        swsco=0
        fmtoi=2
        cadena07=$archivo.ACIERTA;;

     6) scoring="006"
        swsco=0
        fmtoi=2
        cadena07=$archivo.BASICO;;

     8) scoring="008"
        swsco=0
        fmtoi=2
        cadena07=$archivo.ACI.EME;;

     41) scoring="041"
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.AADHVT;;

     41M) scoring="041"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADHVT;;

     41m) scoring="041"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADHVT;;

     45) scoring="045"
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.AADCOO;;

     45M) scoring="045"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADCOO;;

     45m) scoring="045"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADCOO;;

     47) scoring="047"
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.AADFGE;;

     47M) scoring="047"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADFGE;;

     47m) scoring="047"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADFGE;;

     48) scoring="048"
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.AADTRO;;

     48M) scoring="048"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADTRO;;

     48m) scoring="048"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADTRO;;

     49) scoring="049"
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.ACIEAA;;

     49M) scoring="049"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.ACIEAA;;

     49m) scoring="049"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.ACIEAA;;

     62) scoring="062"
        swsco=2
        fmtoi=1
        mnto=""
        cadena07=$archivo.QUANTO;;

     62M) scoring="062"
        swsco=2
        fmtoi=1
        mnto="MANTENIMIENTO"
        cadena07=$archivo.QUANTO;;

     62m) scoring="062"
        swsco=2
        fmtoi=1
        mnto="MANTENIMIENTO"
        cadena07=$archivo.QUANTO;;

     67) scoring="067"
        swsco=0
        fmtoi=2
        mnto=""
        cadena07=$archivo.PREDHD;;

     67M) scoring="067"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.PREDHD;;

     67m) scoring="067"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.PREDHD;;

     95) scoring="095"
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.AADINT;;

     95M) scoring="095"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADINT;;

     95m) scoring="095"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.AADINT;;

     99) scoring="099"
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.CARACT;;

     99M) scoring="099"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.CARACT;;

     99m) scoring="099"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.CARACT;;

     101) cls;
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.A*
        ViewSelectCombos ;;

     119) scoring="119"
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.MICR-B;;

     119M) scoring="119"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.MICR-B;;

     119m) scoring="119"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        cadena07=$archivo.MICR-B;;

     120) scoring="120"
        swsco=0
        fmtoi=2
        mnto="NO"
        ACI120="S"
        cadena07=$archivo.MICR-E;;

     120M) scoring="120"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        ACI120="S"
        cadena07=$archivo.MICR-E;;

     120m) scoring="120"
        swsco=0
        fmtoi=2
        mnto="MANTENIMIENTO"
        ACI120="S"
        cadena07=$archivo.MICR-E;;

     103) cls;
        swsco=0
        fmtoi=2
        mnto="NO"
        cadena07=$archivo.M*
        ViewSelectCombosM ;;

     115) exit;;
     116) kill 0;;

esac
#
#REQ509: Si QUANTO: formato 1-11-45 se valida para 1-11 no se valida
#REQ962: Si Quanto: formato 1 - 11: busca nombres y valida
#                           1 - 11- 45: Valida
# if [ $swsco -eq 2 ]  then
if [ $ACI120 == "S"  ] ; then
   echo "El formato SOLO puede ser: 1-11-45-15 Para SCORE 120 en  " $ACI120
   echo "En donde los ultimos 15 son el valor de activos en pesos "
   echo "Cualquier otra extructura se manejara como BURO          "
   echo "Si esta seguro precione <ENTER> para continuar           "
   read x
   fmtoid=2
   fmtoi=2
else
  echo "El formato puede ser: 1-11(Busca nombres y valida)  o"
  echo "                      1-11-45(se valida)"
  echo "Digite 1 para formato 1-11 o 2 para 1-11-45  -----> \c"
  read fmtoid
  if test -z "$fmtoid"
   then
    fmtoid=2
    fmtoi=2
  else
    if [ $fmtoid -eq 2 ]; then
       fmtoi=2
    else
       fmtoi=1
    fi
  fi
fi
echo "Formato de Archivo Ingresado: " $fmtoid
# fi
##
cd $TEMPORALES
if [ $fmtoi -eq 2 ]; then
  if test -s $cadena01
   then
     echo
     echo "Procesando archivo PR 1-11-45 ---> "$cadena01
  else
     echo "!!!...Archivo $cadena01 no existe proceso termina ....!!!"
     echo
     exit
  fi
else
  if test -s $cadena01
   then
     echo
     echo "Procesando archivo 1-11 ---> " $cadena01
  else
     echo "!!!...Archivo $cadena01  no existe proceso termina ....!!!"
     echo
     exit
  fi
fi
#Req.466: para fecha anterior se hace cambio de ambiente
fec_hoy=`date '+%Y%m%d'`
AAAA=`echo $fec_hoy | cut -c 1-4`
MM=`echo $fec_hoy | cut -c 5-6`
fechahoy=$AAAA$MM
if [ $fechabase -lt $fechahoy ]; then
 cd $DATOS
 sanata=`grep $fechabase ICSANATA.DAT | cut -f2 -d"/"|cut -f3 -d"_"`
 echo "sanata para fecha " $fechabase "  es " $sanata
 if test -z "$sanata"
  then
  echo " !!!!   error buscando sanata   !!!  "
  echo "Digite san_ata para fecha historica $fechabase  ---> \c"
  read sanata
 fi
fi
#FIN REQ509
cd $TEMPORALES

### [[[  DLM - 2014-05-26
export OPC_STABILITY=" "
until [ "$OPC_STABILITY" == "N" -o "$OPC_STABILITY" == "S" ]
do
  echo "\nProceso Stability?   S/N --->\c"
  read ans
  OPC_STABILITY=`echo $ans | tr -d '[:digit:]' | tr -s '[:lower:]' '[:upper:]'`
done
### ]]]  DLM - 2014-05-26
echo "\n"

RevisaNit
echo "*-------------------------------------------------------------*"
echo "Digite: 1. Verificar Nombre Completo"
echo "Digite: 2. Verificar Primer Apellido"
echo "*-------------------------------------------------------------*"
echo "Entre Opcion -----> \c"
read opcion01
echo "*-------------------------------------------------------------*"
echo "*   PROCESO: SCORE $nscore EN BATCH  FECHA BASE $fechabase    *"
echo "                                                        "
if [ $fmtoi -eq 1 ]; then
   numero01=`wc -l < $cadena01`
   echo "      Registros iniciales         -->  " $numero01
   echo "Ejecuta programa CTL100   "
   echo "   PESVNO - Asume Funcion de Nombres"
   cat $DATOS/REGPESVNO.VALI $cadena01 >| $cadena011
   wc -l $cadena01 $cadena011
   echo
   cp $cadena011 $cadena01
#   nohup x CTL100 $archivo >| $cadena06 2>> $cadena06
   fmtoi=2
fi
if [ $fmtoi -eq 2 ]; then
#   echo "   PESVNO - Asume Funcion de Nombres"
#   cat $DATOS/REGPESVNO.VALI $cadena01 >| $cadena011
#   wc -l $cadena01 $cadena011
#   echo
#   cp $cadena011 $cadena01

   if [ $opcion01 -gt 1 ]
   then
      echo "Validando Primer apellido  ---> "$cadena01
      nohup x PESVNO $cadena01 $cadena02 $cadena03 10 1 >| $cadena06 2>> $cadena06
   else
      echo "Validando Nombre Completo ----> "$cadena01
      nohup x PESVNO $cadena01 $cadena02 $cadena03 10 2 >| $cadena06 2>> $cadena06
   fi
   echo " "
   numero01=`wc -l < $cadena01`
   numero02=`wc -l < $cadena02`
   numero03=`wc -l < $cadena03`
   echo "      +---------------------------------------------+"
   echo "      |     ** RESULTADOS DE VALIDACION **          |"
   echo "      +---------------------------------------------+"
   echo "      | Archivo   Procesado   : " $cadena01
   echo "      +---------------------------------------------+"
   echo "      | Registros Validos           -->  " $numero02
   echo "      | Registros Inconsistencias   -->  " $numero03
   echo "      |                                              "
   echo "      | Registros Procesados        -->  " $numero01
   echo "      +---------------------------------------------+"
   echo
   grep Registros  $cadena06
   #
#  nawk '{ printf("%s%s%s%s%s%s%s\n",substr($0,1,1)," ",substr($0,2,11)," ",substr($0,13,45)," ","**")
   nawk '{ printf("%s%s%s%s%s%s%s%s\n",substr($0,1,1)," ",substr($0,2,11)," ",substr($0,13,45),"   ",substr($0,107,15),"**")
}' $cadena02 > $cadena04
   formato="AWK"
else
   numero01=`wc -l < $archivo`
   echo "      Registros iniciales         -->  " $numero01
   formato="IDE"
#  nawk '{ printf("%s%s%s%s%s%s%s\n",substr($0,1,1)," ",substr($0,2,11)," ",substr($0,13,45)," ","**")
#}' $cadena01 > $cadena04
   cp $archivo $cadena04
fi
#
#formato="AWK"
echo
echo
#Req.466: para fecha anterior se hace cambio de ambiente
if [ $fechabase -lt $fechahoy ]; then
   echo "Se hace cambio de ambiente "
   . /san_ata_$sanata/$fechabase/sh-$fechabase
   echo $DATABASE
fi
cd $TEMPORALES
echo "+------------------------------------------+"
echo "|  Ejecucion Programa   SCOTBATCH          |"
echo "+------------------------------------------+"
#
echo $cadena07
#
if [ $opcion -eq 101 ];
  then
    ExecuteSCOTBATCHcombo
else
   if [ $opcion -eq 103 ];
    then
      ExecuteSCOTBATCHcomboM
   else
    SCORELLENO="000000000000000000000000"
    nohup x SCOTBATCH $fechabase $scoring$SCORELLENO $formato $cadena04 $mnto $nitsus >> $cadena06 2>> $cadena06
   fi
fi
tail -22 $cadena06
echo
if [ $swsco -lt 2 ]
 then
   numero04=`grep -cv 00000............$ $cadena07`
else
   numero04=`grep -cv ^..............0000000000 $cadena07`
fi
#
#REQ590
#echo "+-------------------------------------------------------------+"
#echo "| Registros con SCORE  mayor a ceros:  " $numero04
#echo "+-------------------------------------------------------------+"
#REQ590
#REQ509
numero04=`wc -l < $cadena07`
numero05=`wc -l < $cadena03`
echo "+-------------------------------------------------------------+"
echo "| Archivo de salida del proceso:  " $cadena07
echo "| Archivo de inconsistencias   :  " $cadena03
echo "+-------------------------------------------------------------+"
#REQ509
echo
echo "        (ENTER) Continuar !!!  "
read xxx
exit
/d/iccol/desarrollo/macros>

