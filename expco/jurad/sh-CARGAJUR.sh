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
NOHUP="nohup"
codsus="000001"
P_REAL_FALSO="1"
P_PROCESO="#";
P_NombreArchivo=""


GetFechaSistema()
{
#*-----------------------------------------------------------------------------*
# Arma y muestra la fecha del sistema y la m�quina
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
        echo "             opci�n (123)   ---> \c"
        read P_REAL_FALSO
        if [ $P_REAL_FALSO != "1" ] && [ $P_REAL_FALSO != "2" ] && [ $P_REAL_FALSO != "3" ]
        then
            echo "\n\tlas opciones v�lidas son 1, 2 o 3"
        else
            endLoop=1
        fi
    done
}

GetTipoDeProceso()
{
#*-----------------------------------------------------------------------------*
# Averigua el tipo de proceso: carga inicial, actualizaci�n, refresque total ...
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
        echo "           opci�n (01 02 03 04 00)   ---> \c"
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
#               est� en el nombre del arch
#               separa los dos caractares justo antes de la extensi�n ".txt"
                auxTxt=`echo $P_NombreArchivo | sed 's/.*\\(..\\)\\.txt/\\1/'`
#               echo $auxTxt " deber�a ser " $P_PROCESO
                if [ $P_PROCESO == $auxTxt ]
                then
#                   el c�digo de suscriptor al principio del nombre del
#                   archivo debe ser el de JURAD
                    auxTxt=`echo $P_NombreArchivo | sed 's/\\(......\\).*/\\1/'`
#                   echo $auxTxt " deber�a ser " $codsus
                    if [ $codsus == $auxTxt ]
                    then
                        endLoop=1
                    else
                        echo "El c�digo de suscriptor en el nombre del archivo no coincide"
                    fi
                else
                    echo "El nombre del archivo no condice con el tipo de proceso" $P_PROCESO
                fi
            fi
        fi
    done
}

ejecutar_validacion() {
#*******************************************************************************
# PESVNO: validaci�n de IDs
# Valida los registros del archivo .prn y graba archivos .val y .inc, el listado
# en un archivo -log
# Los nombres de los archivos est�n en los par�metros 1 a 4
#*******************************************************************************
 echo ">>>>>>>>>>>> ejecutando validaci�n"
    archivo_prn=$1
    archivo_val=$2
    archivo_inc=$3
    archivo_log=$4
    echo PESVNO $archivo_prn $archivo_val $archivo_inc 10 2
    $NOHUP x PESVNO $archivo_prn $archivo_val $archivo_inc 10 2 >| $archivo_log
#   x PESVNO $na.prn $na.val $na.inc 10 2
    cantDeRegistrosTotal=$(wc -l < $archivo_prn)
    cantDeRegistrosVal=$(wc -l < $archivo_val)
    cantDeRegistrosInc=$(wc -l < $archivo_inc)
    echo "PESVNO - registros v�lidos:" $cantDeRegistrosVal " inconsistentes:" $cantDeRegistrosInc " total:" $cantDeRegistrosTotal
    tail -20 $archivo.log
    if [[ $cantDeRegistrosTotal -ne $(expr $cantDeRegistrosVal + $cantDeRegistrosInc ) ]]
    then
        echo " "
        echo $raya
        echo "Las cantidades de registros no cuadran: proceso cancelado"
        echo $raya
        # cancelado="1"
    fi
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

#*******************************************************************************
# Banner de inicio
#*******************************************************************************
    echo " "
    echo $raya
    echo " "
    banner CARGAJUR
    echo " "
    cd $TEMPORALES
#*******************************************************************************
# Validaci�n de IDs con PESVNO
#*******************************************************************************
    echo $raya
    echo "VALIDACI�N DE IDs"
    echo $raya
    echo " "
    # arma el archivo .prn con los tipo y n�meros de id, sin el registro "T"
    PVNO_prn="PVNO$$.prn"
    grep -v "^T" $P_NombreArchivo | cut -c2-13  >| $PVNO_prn
#   grep -v "^T" $na.txt | cut -c2-13  >| $na.prn
    PVNO_val="PVNO$$.val"
    PVNO_inc="PVNO$$.inc"
    PVNO_log="PVNO$$.log"
    ejecutar_validacion( $PVNO_prn $PVNO_val $PVNO_inc $PVNO_log )
    # muestra el log del step y lo agrega al del proceso
    cat $PVNO_log
    cat $PVNO_log >> $P_NombreArchivo.log
    echo " "

#*******************************************************************************
    echo $raya
    echo "CARGAJUR Carga de los datos de JURAD en BDIIALE"
    echo $raya
    echo " "
    echo " Procesando $P_NombreArchivo "
    echo "real:" $P_REAL_FALSO " tipo de proceso:" $P_PROCESO
    $NOHUP x CARGAJUR $P_NombreArchivo $P_REAL_FALSO >| $P_NombreArchivo.log 2>| $TEMPORALES/$P_NombreArchivo.log
    echo $raya
    echo "             RESULTADOS DEL PROCESO"
    echo " "
    cat $P_NombreArchivo.log
    echo " "
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


