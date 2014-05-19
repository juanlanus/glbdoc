#sh-CARGAJUR.v.01.0001
#**********************************************************
#ENTIDAD:      DATACREDITO                                *
#SHELL-ID:     sh-CARGAJUR                                *
#DATE-WRITTEN: MAY/2014                                   *
#AUTHOR:       GLOBANT                                    *
#******************************************************************
#            | CQnnnn - CARGA DE ARCHIVO EXTERNO                  *
# 16-may-2014|                                                    *
# Globant.   |                                                    *
#            |                                                    *
#******************************************************************
#
#*========================================================*
#*                DEFINICION DE VARIABLES                 *
#*========================================================*
#
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

GetProceso()
{
#*-----------------------------------------------------------------------------*
# PREGUNTA SI EL PROCESO SE REALIZA
#
#*-----------------------------------------------------------------------------*
    P_FCORTE=$FECHOY_ANOMES
    P_PROCESO="#";
    while [ $P_PROCESO != "S" ] && [ $P_PROCESO != "N" ] ; do
          echo
          echo "*--------------------------------------------------------*"
          echo "     **  DEFINICION DE LA BASE DATOS PARA PROCESO **     "
          echo "*--------------------------------------------------------*"
          echo "           <S>   Proceso Real                   "
          echo "           <N>   Proceso En Falso               "
          echo "*--------------------------------------------------------*"
          echo "       Digite opcion  (S/N)   ---> \c"
          read P_PROCESO

          if [ -z "$P_PROCESO" ] ; then
              P_PROCESO="N"
          else
               if [ $P_PROCESO == "S" ] || [ $P_PROCESO == "s" ] ; then
                   P_PROCESO="S"
               else
                   if [ $P_PROCESO == "N" ] || [ $P_PROCESO == "n" ] ; then
                       P_PROCESO="N"
                   else
                       echo "\n\t*ERRROR* OPCION NO VALIDA! <ENTER>\c"
                       GetEnter
                   fi
               fi
          fi
    done
}

GetPeriodoSanata()
{
#*-----------------------------------------------------------------------------*
# PREGUNTAR POR EL PERIODO DEL SANATA Y EFECTUAR EL CAMBIO
#*-----------------------------------------------------------------------------*
    echo "     Digite Fecha de la Base de Datos (AAAAMMDD) a Procesar  ---> \c"
    read FECHIS_ANOMESDIA
    longitud=`echo $FECHIS_ANOMESDIA | tr -d '[:alpha:]' | awk '{printf("%d\n", length($0))}'`
    if [ $longitud -ne 8 ] && [ $longitud -ne 0 ] ; then
          echo "\t *ERROR*  Periodo Historico NO válida. La Fecha debe ser (AAAAMMDD) -Proceso cancelado-"
          GetEnter
          exit
    else
        if [ $longitud -eq 0 ] ; then
             P_FCORTE=$FECHOY_ANOMES
             FECHIS_ANOMESDIA=`echo $FECHOY_ANOMES`
        else
             cd $DATOS
             FECHIS_ANOMES=`echo $FECHIS_ANOMESDIA | cut -c 1-6`
             sanata=`grep $FECHIS_ANOMES ICSANATA.DAT | cut -f2 -d"/"|cut -f3 -d"_"`
             cd $TEMPORALES
             echo "Sanata para Fecha " $FECHIS_ANOMESDIA "  es " $sanata
             if test -z "$sanata" ; then
                  P_FCORTE=$FECHOY_ANOMES
                  echo "\n !!!! *ERROR*  sanata de $FECHIS_ANOMESDIA NO Existe !!! \n "
                  echo "                   **** PROCESO CANCELADO **** "
                  GetEnter
                  exit
             else
                  P_FCORTE=$FECHIS_ANOMES
                  CambiarSanata
             fi
         fi
    fi
}

CambiarSanata()
{
#*-----------------------------------------------------------------------------*
# VERIFICAR Y CAMBIAR AL SANATA CORRESPONDIENTE
#*-----------------------------------------------------------------------------*
   if [ $FECHOY_ANOMES -ne $FECHIS_ANOMES ] ; then
         echo "\n\tSe Procesa con fecha $FECHIS_ANOMESDIA \n"
          . /san_ata_$sanata/$FECHIS_ANOMES/sh-$FECHIS_ANOMES
          echo $DATABASE
          P_FCORTE=$FECHIS_ANOMES
   else
          P_FCORTE=$FECHOY_ANOMES
   fi
}


#* PARAMETROS:
fechab=0;
codsus=0;
nomsus={};
opcion="A";
vector=48;
numeroreg=0;
cadena01={};
cadena02={};
cadena03={};
cadena04={};
cadena05={};
cadena06={};
cadena07={};
cadena08={};
cadena09={};
cadena10={};
cadena11={};
#*========================================================*
#*                    PROCESO                             *
#*========================================================*
#*              FASE 1: PIDE PARAMETROS                   *
#*========================================================*
#*-----------------------------------------------------------------------------*
#                   MENU PRINCIPAL DE EJECUCION
#*-----------------------------------------------------------------------------*
clear
GetFechaSistema
GetProceso

loop=0
while [ $loop != 1 ]
 do
  echo "     Digite Codigo Suscriptor  ---> \c"
  read codsus
  if [ $codsus -eq 999999 ]
   then
    loop=1
  else
   cd $DATABASE
   nomsus=`grep ^"$codsus" ICBSUS | cut -c9-48`
   echo "Codigo suscriptor: " $codsus  "   Suscriptor: " $nomsus
   if test -z "$nomsus"
    then
     echo "suscriptor errado"
   else
     echo "si correcto digite ENTER, si va a modificar digite N  ---> \c"
     read loopd
     if test -z "$loopd"
      then
       loop=1
     fi
   fi
  fi
done
echo "     Digite nombre del archivo     ---> \c"
read archivo
cd $TEMPORALES
if test -s $archivo
then
   echo
   echo "Procesando archivo-----> "$archivo
else
   echo "!!!...Archivo $archivo  no existe proceso termina ....!!!"
   echo
   echo "        (ENTER) Continuar !!!  "
   read xxx
   exit
fi
echo
echo "+-------------------------------------------------------------+"
echo "+-------------------------------------------------------------+"
echo "     Fecha base (AAAAMM)     -----> ($P_FCORTE) "
fechab=$P_FCORTE
echo "+-------------------------------------------------------------+"
numeroreg=`wc -l < $archivo`
echo
echo "+-------------------------------------------------------------+"
echo "| Registros iniciales archivo de entrada : " $numeroreg
echo "+-------------------------------------------------------------+"
cadena00=$archivo.orig
cp $archivo $cadena00
echo
#echo "+-------------------------------------------------------------+"
#echo "|                 SEGUNDA FASE: PROCESO                       |"
#echo "+-------------------------------------------------------------+"
#*========================================================*
#*  EXECUTE PROGRAM: icestdatVEC-NORMAL                   *
#*========================================================*
#
echo
echo "+------------------------------------------------------+"
echo "|  Programa CARGAJUR                                   |"
echo "|                                                      |"
echo "|  Procesando archivo --> "  $archivo
echo "+------------------------------------------------------+"
echo "+-------------------------------------------------------------+"
echo "|     *********  RESULTADOS DEL PROCESO       **********      |"
echo "|                                                             |"
ls -lrt $archivo* >> $archivo.log
tail -20 $archivo.log
echo "+-------------------------------------------------------------+"

