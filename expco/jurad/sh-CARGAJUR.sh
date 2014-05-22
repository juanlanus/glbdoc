#sh-CARGAJUR.v.01.0001 grabado /d/iccol/desarrollo/macros/sh-CARGAJUR
#**********************************************************
#ENTIDAD:      DATACREDITO                                *
#SHELL-ID:     sh-CARGAJUR                                *
#DATE-WRITTEN: MAY/2014                                   *
#AUTHOR:       GLOBANT                                    *
#******************************************************************
# 16-may-2014| CQnnnn - CARGA DE ARCHIVO EXTERNO                  *
# Globant.   |                                                    *
#******************************************************************
#
#*========================================================*
#*          DEFINICION DE VARIABLES Y FUNCIONES
#*========================================================*
raya="---------------------------------------------------"
codsus="000001"
P_REAL_FALSO="1"
P_PROCESO="#";
P_NombreArchivo=""


GetFechaSistema()
{
#*-----------------------------------------------------------------------------*
# Arma y muestra la fecha del sistema y la máquina
#*-----------------------------------------------------------------------------*
    P_SERVER_DEV="172.24.6.154"
    HoraHoy=`date '+%H:%M'`
    FECHOY_A4MMDD=`date '+%Y%m%d'`
    FECHOY_ANOMES=`date '+%Y%m%d' | cut -c 1-6`
    P_FCORTE=$FECHOY_ANOMES
    P_MAQUINA=`who am i | cut -c 39-50`
    P_FCORTE=$FECHOY_ANOMES
    echo $raya
    echo " "
    echo "                D A T A C R E D I T O"
    echo " "
    echo "              Cargue de archivo de JURAD"
    echo " "
    echo "Fecha:" $FECHOY_A4MMDD $HoraHoy " en" $P_MAQUINA
    echo " "
}

GetRealOFalso()
{
#*-----------------------------------------------------------------------------*
# Averigua si el tipo de proceso es REAL o EN FALSO
#*-----------------------------------------------------------------------------*
    echo
    echo $raya
    echo "              PROCESO REAL O EN FALSO"
    echo $raya
    P_REAL_FALSO="#";
    endLoop=0
    while [ $endLoop == 0 ]
    do
#             ---------------------------------------------------
        echo " "
        echo "        1) Proceso en falso"
        echo " "
        echo "        2) Proceso real"
        echo " "
        echo "        3) Salir"
        echo " "
        echo "             opción (123)   ---> \c"
        read P_REAL_FALSO
        if [ $P_REAL_FALSO != "1" ] && [ $P_REAL_FALSO != "2" ] && [ $P_REAL_FALSO != "3" ]
        then
            echo "\n\tlas opciones válidas son 1, 2 o 3"
        else
            endLoop=1
        fi
    done
}

GetTipoDeProceso()
{
#*-----------------------------------------------------------------------------*
# Averigua el tipo de proceso: carga inicial, actualización, refresque total ...
#*-----------------------------------------------------------------------------*
    echo
    echo $raya
    echo "                  TIPO DE PROCESO"
    echo $raya
    endLoop=0
    P_PROCESO="##"
    while [ $endLoop == 0 ]
    do
#             ---------------------------------------------------
        echo " "
        echo "      01) Envio Original  - Carga TOTAL 1a vez"
        echo " "
        echo "      02) Archivo Semanal - Carga PARCIAL"
        echo " "
        echo "      03) Archivo Mensual - Carga PARCIAL"
        echo " "
        echo "      04) Refresque Total - Carga TOTAL"
        echo " "
        echo "      00) Salir"
        echo " "
        echo "           opción (01 02 03 04 00)   ---> \c"
        read P_PROCESO
        if [ -z "$P_PROCESO" ]
        then
            echo "\n\t ingrese una de las opciones 01 02 03 04 00\c"
        else
            if [ $P_PROCESO != "01" ] && [ $P_PROCESO != "02" ] && [ $P_PROCESO != "03" ] && [ $P_PROCESO != "04" ] && [ $P_PROCESO != "00" ] 
            then
                echo "\n\t ingrese una de las opciones 01 02 03 04 00\c"
            else
                endLoop=1
            fi
        fi
    done
}

GetNombreDelArchivo()
{
#*-----------------------------------------------------------------------------*
# Averigua el nombre del archivo
#*-----------------------------------------------------------------------------*
    echo
    echo $raya
    echo "                NOMBRE DEL ARCHIVO"
    echo $raya
    endLoop=0
    while [ $endLoop == 0 ]
    do
#             ---------------------------------------------------
        echo " "
        echo " Ingrese el nombre del archivo o enter para salir:"
        echo "      "$codsus"100ddmmaa01"$P_PROCESO".txt"
#                   JJJJJJaaaDDMMAAbbXX.txt
        echo " ---> \c"
        read P_NombreArchivo
        if [ -z "$P_NombreArchivo" -o "$P_NombreArchivo" == "" ]
        then
            endLoop=1
        else
            if [ ! -s $TEMPORALES"/"$P_NombreArchivo ]
            then
                echo "no se encuentra el archivo " $P_NombreArchivo
            else
#               el tipo de proceso 01/02/03/04 debe coincidir con el que
#               está en el nombre del arch
#               separa los dos caractares justo antes de la extensión ".txt"
                auxTxt=`echo $P_NombreArchivo | sed 's/.*\\(..\\)\\.txt/\\1/'`
#               echo $auxTxt " debería ser " $P_PROCESO
                if [ $P_PROCESO == $auxTxt ]
                then
#                   el código de suscriptor al principio del nombre del
#                   archivo debe ser el de JURAD
                    auxTxt=`echo $P_NombreArchivo | sed 's/\\(......\\).*/\\1/'`
#                   echo $auxTxt " debería ser " $codsus
                    if [ $codsus == $auxTxt ]
                    then
                        endLoop=1
                    else
                        echo "El código de suscriptor en el nombre del archivo no coincide"
                    fi
                else
                    echo "El nombre del archivo no condice con el tipo de proceso" $P_PROCESO
                fi
            fi
        fi
    done
}


#*========================================================*
#*                    PROCESO                             *
#*========================================================*
#*              FASE 1: PIDE PARAMETROS                   *
#*========================================================*
echo "........................................."
clear
GetFechaSistema
GetRealOFalso
if [ $P_REAL_FALSO -eq 3 ]
then
    exit
fi
GetTipoDeProceso
if [ $P_PROCESO -eq 00 ]
then
    exit
fi
GetNombreDelArchivo
if [ -z "$P_NombreArchivo" ]
then
    exit
fi

# echo (esto no está refactoreado)
# echo "+-------------------------------------------------------------+"
# echo "+-------------------------------------------------------------+"
# echo "     Fecha base (AAAAMM)     -----> ($P_FCORTE) "
# fechab=$P_FCORTE
# echo "+-------------------------------------------------------------+"
# numeroreg=`wc -l < $P_NombreArchivo archivo`
# echo
# echo "+-------------------------------------------------------------+"
# echo "| Registros iniciales archivo de entrada : " $numeroreg
# echo "+-------------------------------------------------------------+"
# cadena00=$archivo.orig
# cp $P_NombreArchivo $cadena00
#echo "+-------------------------------------------------------------+"
#echo "|                 SEGUNDA FASE: PROCESO                       |"
#echo "+-------------------------------------------------------------+"
    echo " "
    echo $raya
    echo " "
    banner CARGAJUR
    echo " "
    echo $raya
    echo " Procesando $P_NombreArchivo "
#   echo "real:" $P_REAL_FALSO " tipo de proceso:" $P_PROCESO
    nohup x CARGAJUR $P_NombreArchivo $P_REAL_FALSO >| $TEMPORALES/$P_NombreArchivo.log 2>| $TEMPORALES/$P_NombreArchivo.log
    echo $raya
    echo "             RESULTADOS DEL PROCESO"
    echo " "
    cat $TEMPORALES/$P_NombreArchivo.log
    echo " "
    cd $TEMPORALES
    echo "Archivos en \$TEMPORALES:"
    ls -lrt $P_NombreArchivo* >> $P_NombreArchivo.log
    ls -lrt $P_NombreArchivo*
    cd - > /dev/null
    echo $raya
    echo " "
    echo " "
    echo " "
    echo " "
    echo " "
    echo " "
    echo " "
    echo " "

