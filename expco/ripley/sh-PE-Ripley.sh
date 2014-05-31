# /d/iccol/desarrollo/macros/sh-PE-Ripley
#sh-PE-Ripley.V.1.0001
#*******************************************************************************
#SHELL-ID:     sh-PE-Ripley
#DATE-WRITTEN: 2014/05/23
#LAST-UPDATE:  2014/05/23
#AUTHOR:       Globant - JL
#*******************************************************************************
# Definici�n de constantes
    doCuu="1"                        # reposicionar el cursor luego de un error
    NOHUP=""                   # contiene "nohup" en producci�n
    cancelado="0"                    # las funciones ponen "1" para cancelar todo
    P_SERVER_DEV="codes"            # hostname de la m�quina de develop
    raya="----------------------------------------------------------------------"
    ASCIIBanner="
        ######  #######    ######
        #     # #          #     # # #####  #      ###### #   #
        #     # #          #     # # #    # #      #       # #
        ######  #####      ######  # #    # #      #####    #
        #       #          #   #   # #####  #      #        #
        #       #          #    #  # #      #      #        #
        #       #######    #     # # #      ###### ######   #
"

# Par�metros y sus valores iniciales
    FECHA_PROC=$(date '+%Y%m%d')
#   "I": la lista de IDs es interna  "E": provista x cliente
    BASE="E"
#       si BASE="E": archivo y estructura ("1": 1-11 o "2": 1-11-45)
        NOM_ARCHIVO=" "
        TIPO_ARCHIVO="2"
#       si BASE="I": "N": extracci�n x NIT  "C": por cod suscriptor
        TIPO_EXTRACT="N"
#           si TIPO_EXTRACT="C": NIT (9) o Cod Suscriptor (6), seg�n TIPO_EXTRACT
            ID_EXTRACT=" "
#   Tipo de proceso "A": actual  "H": hist�rico
    TIPO_PROC="A"
#       Si TIPO_PROC="H": la fecha inicio de proceso hist�rico
        FECHA_INICIO=" "

readBASE() {
#*******************************************************************************
# Lee el valor de BASE hasta que es OK {I|C} o el ope cancela
#*******************************************************************************
    loopEnd="0"
    while [ $loopEnd == "0" ]
    do
        echo " "
#             ......................................................................
        echo "                       ORIGEN DE LA BASE"
        echo $raya
        echo "      I) Interna"
        echo "      E) Externa, provista por el Cliente"
        echo "      enter para cancelar"
        echo " "
        echo "            BASE:\c"
        read BASEnew
        if [ -z $BASEnew ]
        then
            loopEnd="1"
            exit
        fi
        [ $BASEnew == "e" ] && BASEnew="E"
        [ $BASEnew == "i" ] && BASEnew="I"
        if [ $BASEnew == "E" ] || [ $BASEnew == "I" ]
        then
            BASE=$BASEnew
            loopEnd="1"
        else
            tput bel
            [ $doCuu == "1" ] &&  tput cuu 8
        fi
    done
}

readNOM_ARCHIVO() {
#*******************************************************************************
# Lee el nombre del archivo de entrada y verifica que exista en $TEMPORALES
#*******************************************************************************
    loopEnd="0"
    while [ $loopEnd == "0" ]
    do
        echo " "
#             ......................................................................
        echo "                       NOMBRE DEL ARCHIVO BASE"
        echo $raya
        echo "      Ingrese el nombre del archivo base,"
        echo "      enter para cancelar"
        echo " "
                tput el
        echo "            ARCHIVO:\c"
        read NOM_ARCHIVOnew
        tput el
        if [ -z $NOM_ARCHIVOnew ]
        then
            loopEnd="1"
            exit
        fi
        if [ -d $NOM_ARCHIVOnew ]
        then
            echo $NOM_ARCHIVOnew" es un nombre de directorio"
            tput bel
            [ $doCuu == "1" ] &&  tput cuu 8
        else
            if [ ! -f $NOM_ARCHIVOnew ]
            then
                echo "El archivo "$NOM_ARCHIVOnew" no se encuentra"
                tput bel
                                [ $doCuu == "1" ] &&  tput cuu 8
            else
                NOM_ARCHIVO=$NOM_ARCHIVOnew
                loopEnd="1"
            fi
        fi
    done
}

readTIPO_ARCHIVO() {
#*******************************************************************************
# Lee el tipo de archivo "1"=1-11  "2"=1-11-45
#*******************************************************************************
    loopEnd="0"
    while [ $loopEnd == "0" ]
    do
        echo " "
#             ......................................................................
        echo "                    ESTRUCTURA DEL ARCHIVO BASE"
        echo $raya
        echo "      1) 1-11"
        echo "      2) 1-11-45"
        echo "      enter para cancelar"
        echo " "
        echo "            ESTRUCTURA:\c"
        read TIPO_ARCHIVOnew
        if [ -z $TIPO_ARCHIVOnew ]
        then
            loopEnd="1"
            exit
        fi
        if [ $TIPO_ARCHIVOnew == "1" ] || [ $TIPO_ARCHIVOnew == "2" ]
        then
            TIPO_ARCHIVO=$TIPO_ARCHIVOnew
            loopEnd="1"
        else
            tput bel
            [ $doCuu == "1" ] &&  tput cuu 8
        fi
    done
}

readTIPO_EXTRACT() {
#*******************************************************************************
# Tipo de extracci�n, cuando BASE="I": "N" por NIT  "C" por cod suscriptor
#*******************************************************************************
    loopEnd="0"
    while [ $loopEnd == "0" ]
    do
        echo " "
#             ......................................................................
        echo "                         TIPO DE EXTRACCI�N"
        echo $raya
        echo "      N) Por NIT"
        echo "      C) Por codigo de suscriptor"
        echo "      enter para cancelar"
        echo " "
        echo "            TIPO de EXTRACCI�N:\c"
        read TIPO_EXTRACTnew
        if [ -z $TIPO_EXTRACTnew ]
        then
            loopEnd="1"
            exit
        fi
        [ $TIPO_EXTRACTnew == "c" ] && TIPO_EXTRACTnew="C"
        [ $TIPO_EXTRACTnew == "n" ] && TIPO_EXTRACTnew="N"
        if [ $TIPO_EXTRACTnew == "N" ] || [ $TIPO_EXTRACTnew == "C" ]
        then
            TIPO_EXTRACT=$TIPO_EXTRACTnew
            loopEnd="1"
        else
            tput bel
            [ $doCuu == "1" ] &&  tput cuu 8
        fi
    done
}

readID_EXTRACT() {
#*******************************************************************************
# Lee el criterio para extracci�n base interna: un NIT o un cod de suscriptor
#*******************************************************************************
#   seg�n TIPO_EXTRAC se lee un NIT o un COD SUSCRIPTOR
    if [ $TIPO_EXTRACT == "N" ]
    then
        criterioExtraccion="        NIT"
        inputLen=11
    else
        criterioExtraccion="COD SUSCRIPTOR"
        inputLen=6
    fi
    loopEnd="0"
    while [ $loopEnd == "0" ]
    do
        echo " "
#             ......................................................................
        echo "                $criterioExtraccion PARA LA EXTRACCI�N"
        echo $raya

        echo "      Ingrese el "$criterioExtraccion" para extracci�n ($inputLen d�gitos),"
        echo "      enter para cancelar"
        echo " "
        tput el                     # borra caracteres sobrantes
        echo "            $criterioExtraccion:\c"
        read ID_EXTRACTnew
        tput el                     # borra mensaje de error
        if [ -z $ID_EXTRACTnew ]
        then
            loopEnd="1"
            exit
        fi
        # debe ser solo n�meros
        if [[ "$ID_EXTRACTnew" !=  +([0-9]) ]]
        then
            echo "el dato ingresado debe ser num�rico"
            tput bel
            [ $doCuu == "1" ] &&  tput cuu 8
        else
            # control de longitud (11 o 6)
            if [ ${#ID_EXTRACTnew} -ne inputLen ]
            then
                echo "longitud incorrecta (debe ser "$inputlen" d�gitos)"
                tput bel
                [ $doCuu == "1" ] &&  tput cuu 8
            else
                ID_EXTRACT=$ID_EXTRACTnew
                loopEnd="1"
            fi
        fi
    done
}

readTIPO_PROC() {
#*******************************************************************************
# Tipo de proceso, "A" por Actual, "H" por Hist�rico
#*******************************************************************************
    loopEnd="0"
    while [ $loopEnd == "0" ]
    do
        echo " "
#             ......................................................................
        echo "                          TIPO DE PROCESO"
        echo $raya
        echo "      A) Proceso actual"
        echo "      H) Proceso hist�rico"
        echo "      enter para cancelar"
        echo " "
        echo "            TIPO de PROCESO:\c"
        read TIPO_PROCnew
        if [ -z $TIPO_PROCnew ]
        then
            loopEnd="1"
            exit
        fi
        [ $TIPO_PROCnew == "a" ] && TIPO_PROCnew="A"
        [ $TIPO_PROCnew == "h" ] && TIPO_PROCnew="H"
        if [ $TIPO_PROCnew == "H" ] || [ $TIPO_PROCnew == "A" ]
        then
            TIPO_PROC=$TIPO_PROCnew
            loopEnd="1"
        else
            tput bel
            [ $doCuu == "1" ] &&  tput cuu 8
        fi
    done
}

readFECHA_INICIO() {
#*******************************************************************************
# Fecha de proceso, cuando tipo de extracci�n es "H"
#*******************************************************************************
    loopEnd="0"
    while [ $loopEnd == "0" ]
    do
        echo " "
#             ......................................................................
        echo "                  FECHA INICIAL DE PROCESO HIST�RICO"
        echo $raya
        echo "      Ingrese fecha inicial de proceso,"
        echo "      enter para cancelar"
        echo " "
        echo "            FECHA INICIO de PROCESO AAAAMMDD:\c"
        read FECHA_INICIOnew
        if [ -z $FECHA_INICIOnew ]
        then
            loopEnd="1"
            exit
        fi
        # debe ser solo n�meros
        if [[ "$FECHA_INICIOnew" != +([0-9]) ]]
        then
            echo "la fecha solamente puede contener d�gitos"
            tput bel
            [ $doCuu == "1" ] && tput cuu 8
    else
            # rango de a�o de 2000 hasta 2029, mes 01 a 12 y d�a 00 a 31
            # [[ "20141229" == +(20[012][0-9](0[0-9]|1[12])([012][0-9]|3[01])) ]] && echo "ok"
            if [[ "$FECHA_INICIOnew" != +(20[012][0-9](0[0-9]|1[12])([012][0-9]|3[01])) ]]
            then
                echo "la fecha es inv�lida, anterior a 2000 o posterior a 2029"
                tput bel
                [ $doCuu == "1" ] &&  tput cuu 8
            else
                # debe ser menor que la fecha actual (mes o a�o)
                FECHA_INICIOnewYYYYMM=$(echo $FECHA_INICIOnew  | cut -c1-6)
                FECHA_PROC_YYYYMM=$(echo $FECHA_PROC  | cut -c1-6)
                if [ $FECHA_INICIOnewYYYYMM -ge $FECHA_PROC_YYYYMM ]
                then
                    echo "la fecha desde debe ser anterior a la actual"
                    tput bel
                    [ $doCuu == "1" ] &&  tput cuu 8
                else
                    # todo bien
                    FECHA_INICIO=$FECHA_INICIOnew
                    FECHA_INICIO_YYYYMM=$FECHA_INICIOnewYYYYMM
                    loopEnd="1"
                fi
            fi
        fi
    done
}




paramsGet() {
#*******************************************************************************
# Lee los par�metros de la corrida anterior
#*******************************************************************************
#   asegura que exista el arch de par�metros general
    [ ! -f $0.parm ] && touch $0.parm
#   lee los par�metros
    params=$(cat $0.parm)
    echo '>>>> params: ' "$params"
#   separa los par�metros seg�n su posici�n
    BASE=$(echo "$params" | cut -d ";" -f1)
    NOM_ARCHIVO=$(echo "$params" | cut -d ";" -f2)
    TIPO_ARCHIVO=$(echo "$params" | cut -d ";" -f3)
    TIPO_EXTRACT=$(echo "$params" | cut -d ";" -f4)
    ID_EXTRACT=$(echo "$params" | cut -d ";" -f5)
    TIPO_PROC=$(echo "$params" | cut -d ";" -f6)
    FECHA_INICIO=$(echo "$params" | cut -d ";" -f7)
}

paramsPut() {
#*******************************************************************************
# Guarda los par�metros de la corrida en el archivo .parm
#*******************************************************************************
        allParms=$BASE";"${NOM_ARCHIVO:-"N/A"}";"${TIPO_ARCHIVO:-"N/A"}";"
        allParms=$allParms${TIPO_EXTRACT:-"N/A"}";"${ID_EXTRACT:-"N/A"}";"
        allParms=$allParms$TIPO_PROC";"${FECHA_INICIO:-"N/A"}
        echo $allParms
        echo "file:" $0.parm " in:" $(pwd)
        echo $allParms >| $0.parm                       # para pruebas
        echo $allParms >| $archivo.parm
}

paramsPrint() {
#*******************************************************************************
# Muestra todos los par�metros, para pruebas
#*******************************************************************************
    echo "BASE="$BASE"<"
    echo "NOM_ARCHIVO="$NOM_ARCHIVO"<"
    echo "TIPO_ARCHIVO="$TIPO_ARCHIVO"<"
    echo "TIPO_EXTRACT="$TIPO_EXTRACT"<"
    echo "ID_EXTRACT="$ID_EXTRACT"<"
    echo "TIPO_PROC="$TIPO_PROC"<"
    echo "FECHA_INICIO="$FECHA_INICIO"<"
}

paramsDisplay() {
#*******************************************************************************
# Hace un listado de los par�metros calidad presentaci�n
# Ejemplo:
# PAR�METROS del PROCESO
#     Fecha: 20140529                                  FECHA_PROC
#     Base: C  provista por el cliente
#     Archivo: 1
#     Estructura: 1  1-11-45
#     Tipo de proceso: H  hist�rico, desde: 20140101   FECHA_DESDE
#*******************************************************************************
    i='    ' # indent
    s='  '   # separaci�n
    echo "PAR�METROS del PROCESO"
    echo "${i}Fecha="$FECHA_PROC
    echo "${i}Archivo: "$NOM_ARCHIVO
    print -n "${i}Base: "$BASE
    if [ $BASE == "E" ]
    then
        echo "${s}provista por el cliente"
        print -n "${i}Estructura: "$TIPO_ARCHIVO
        [ $TIPO_ARCHIVO == "1" ] && echo "${s}1-11"
        [ $TIPO_ARCHIVO == "2" ] && echo "${s}1-11-45"
    else
        echo "${s}extra�da de datos internos"
        print -n "${i}Criterio de extracci�n: "$TIPO_EXTRACT
        [ $TIPO_EXTRACT == "N" ] && echo "${s}por NIT "$ID_EXTRACT
        [ $TIPO_EXTRACT == "C" ] && echo "${s}por cod suscriptor "$ID_EXTRACT
    fi
    print -n "${i}Tipo de proceso: "$TIPO_PROC
    if [ $TIPO_PROC == "H" ]
    then
        echo "${s}hist�rico, desde: "$FECHA_INICIO
    else
        echo "${s}actual"
    fi
}

ejecutar_extraccion() {
#***********************************************************************
# Extrae registros por suscriptor del archivo ICMCRECOPY.DAT en el prn
#***********************************************************************
    # el directorio del archivo cambia seg�n el ambiente
    P_MAQUINA=$(hostname)
    if [ $P_MAQUINA == $P_SERVER_DEV ]
    then
        echo "M�quina de desarrollo:" $P_MAQUINA
        ICMCRECOPY='/despeciales/ICMCRECOPY.DAT'
    else
        # echo "M�quina de producci�n:" $P_MAQUINA
        ICMCRECOPY='$ESPECIALES/ctlc/ICMCRECOPY.DAT'
    fi
    # extrae tipo y n�mero de id por NIT del suscriptor, elimina repeticiones
    # $$$$ NIT para pruebas: 00900047981
    echo "el archivo de salida es "$archivo_prn " el input es "$ICMCRECOPY
    grepRegex="^A......[14].*"$ID_EXTRACT".P$"
    # echo "regex:"$grepRegex
    [ -s $archivo_prn ] && rm $archivo_prn
    grep "^A......[14].*"$ID_EXTRACT".P$" $ICMCRECOPY | cut -c8-19 | sort -u >| $archivo_prn
    # cuenta los registros extra�dos
    cantRegsExtraidos=$(wc -l < $archivo_prn)
    echo "Registros extra�dos: "$cantRegsExtraidos
    TIPO_ARCHIVO="1"  # es un archivo 1-11
}

ejecutar_validacion() {
#*******************************************************************************
# PESVNO: validaci�n de IDs
# Valida los registtros del archivo .prn y graba archivos .val y .inc
#*******************************************************************************
    echo "Input PESVNO: "$archivo_prn
    # si el input tiene estructura 1-11 le antepone el REGPESVNO.VALI
    if [ $TIPO_ARCHIVO == "1" ]     # el input tiene estructura 1-11
    then
        PESVNO_INPUT="archivo_prn_VALI"
        cat $DATOS/REGPESVNO.VALI $archivo_prn >| archivo_prn_VALI
    else
        PESVNO_INPUT=$archivo_prn
    fi

    print - PESVNO $PESVNO_INPUT $archivo_val $archivo_inc 10 2
    $NOHUP x PESVNO $PESVNO_INPUT $archivo_val $archivo_inc 10 2 >| $archivo.log
    [ -s archivo_prn_VALI ] && rm archivo_prn_VALI
    cantDeRegistrosTotal=$(wc -l < $archivo_prn)
    cantDeRegistrosVal=$(wc -l < $archivo_val)
    cantDeRegistrosInc=$(wc -l < $archivo_inc)
    echo "PESVNO - registros v�lidos:" $cantDeRegistrosVal " inconsistentes:" $cantDeRegistrosInc " total:" $cantDeRegistrosTotal
    head -10 $archivo.log
    if [ $cantDeRegistrosTotal -ne $(expr $cantDeRegistrosVal + $cantDeRegistrosInc) ]
    then
        echo " "
        echo $raya
        echo "Las cantidades de registros no cuadran: proceso cancelado"
        echo $raya
        cancelado="1"
    fi
}

#*******************************************************************************
#*******************************************************************************
#                                     MAIN
#*******************************************************************************
#*******************************************************************************
    #Fecha y hora de ejecuci�n
    horaInicio=$(date '+%H:%M:%S')
    FECHA_PROC=$(date '+%Y%m%d')
    FECHA_PROC_YYYYMM=$(echo $FECHA_PROC  | cut -c1-6)

    clear
    echo $raya
#   banner "PE Ripley"
    echo "$ASCIIBanner"
    echo $raya

#*******************************************************************************
# Lectura de par�metros en la terminal
#   carga los valores del archivo .parm
    paramsGet
#   lee el set de par�metros hasta que sea OK
    parmsOK="0"
    while [ $parmsOK == "0" ]
    do
        readBASE                     # I interna, C cliente
        if [ $BASE == "E" ]
        then
            readNOM_ARCHIVO          # nombre del archivo base
            readTIPO_ARCHIVO         # 0 1-11, 1 1-11-45
        else
            readTIPO_EXTRACT         # N NIT, C cod subs
            readID_EXTRACT           # NIT o cos subs
        fi
        readTIPO_PROC                # A actual, H hist�rico
        if [ $TIPO_PROC == "H" ]
        then
            readFECHA_INICIO           # fecha desde
        else
            FECHA_INICIO=$FECHA_PROC
        fi

        paramsPrint # $$$$ DEBUG
        sleep 2
        # guarda en archivo .parm
        paramsPut
        clear
        echo $raya
        # banner "PE Ripley"
        echo "$ASCIIBanner"
        echo $raya
        paramsDisplay
        echo $raya
        # pregunta al ope si est� satisfecho
        continuar="_"
        while [ $continuar == "_" ]
        do
            echo " "
            echo " "
            echo " "
            echo "Ingrese 1 para cambiar los par�metros, enter para continuar:\c"
            read continuar
            if [ -z $continuar ]
            then
                parmsOK="1"
                continuar="listo"
            else
                if [ $continuar == "1" ]
                then
                    :
                fi
            fi
        done
    done

#*******************************************************************************
# Nombres de los archivos, SANATA
#*******************************************************************************
    # la variable archivo es el nombre de la base, interna o del cliente,
    # y se usa para armar todos los dem�s nombres de archivos del proceso
    if [ $BASE == "E" ]
    then
        # el nombre del archivo provisto, existente
        archivo=$NOM_ARCHIVO
    else
        # un nombre armado con "PERipley" y la fecha del d�a
        # $$$$ no ser� posible ejecutar dos procesos en el mismo d�a
        # $$$$ se puede agregar un $$ al file name ...
        # $$$$ se puede controlar si ya existe un archivo de hoy ...
        archivo="PERipley"$FECHA_PROC
        NOM_ARCHIVO=$archivo
    fi
    # arma los nombres de los archivos del proceso en base al anterior
    archivo_prn=$archivo.prn
    archivo_val=$archivo.val
    archivo_inc=$archivo.inc
    archivo_txt=$archivo.txt
    archivo_DATINF=$archivo.DATINF
    archivo_ESTADI=$archivo.ESTADI
    archivo_QUANTO=$archivo.QUANTO
    archivo_icgs65=$archivo.icgs65
    archivo_OK=$cadena12.OK
    archivo_LOG_ICGS65_fecha="LOG-ICGS65-"$fecha
    archivo_LOG_ICG65_PRB_fecha="LOG-ICGS65-PRB-"$fecha
    archivo_par=$archivo.par

#*******************************************************************************
# Proceso hist�rico: ubica la SANATA que corresponde a la fecha inicial
# de proceso FECHA_INICIO
# Ejemplos de variables exportadas:
# DATABASE=/san_ata_1/200212/icdb
# DATABASE=/san_ata_2/200412/icdb
# DATABASE=/san_ata_3/200512/icdb
# EXTFH=/san_ata_2/200505/extfh.cfg
# EXTFH=/san_ata_2/200504/extfh.cfg
# EXTFH=/san_ata_3/200603/extfh.cfg
# EXTFH=/san_ata_3/200601/extfh.cfg
#*******************************************************************************
    if [ $TIPO_PROC == "H" ]
    then
        FECHA_INICIO_YYYYMM=$(echo $FECHA_INICIO | cut -c1-6 )
        sanata=$(grep $FECHA_INICIO_YYYYMM $DATOS/ICSANATA.DAT | cut -c17-17)
        echo "sanata para fecha" $FECHA_INICIO_YYYYMM "es" $sanata
        if [ -z "$sanata" ]
        then
            echo " "
            echo "No se encontr� sanata para la fecha $FECHA_INICIO_YYYYMM"
            echo "Digite san_ata para fecha historica $FECHA_INICIO_YYYYMM ---> \c"
            read sanata
            echo " "
            export EXTFH="/san_ata_"$sanata"/"$fecha"/extfh.cfg"
            echo $EXTFH
            export DATABASE="/san_ata_"$sanata"/"$fecha"/icdb"
            echo $DATABASE
        fi
    fi

#*******************************************************************************
# Extracci�n
#*******************************************************************************
    cd $TEMPORALES
    if [ $BASE = "I" ]
    then
        echo $raya
        echo "EXTRACCI�N:" $TIPO_EXTRACT " " $ID_EXTRACT
        ejecutar_extraccion
        echo "Finalizada la extracci�n"
    fi

    [ -s $archivo.log ] && rm $archivo.log

#*******************************************************************************
# PESVNO: validaci�n de IDs
# Valida los registros del archivo .prn y graba archivos .val y .inc
#*******************************************************************************
    [ -s $archivo_val ] && rm $archivo_val
    [ -s $archivo_inc ] && rm $archivo_inc
    echo $raya
    echo "PESVNO: validaci�n de IDs"
    ejecutar_validacion
    [ $cancelado == "1" ] && exit

    # control: si no hay un archivo .val se cancela el proceso
    if [ ! -s $archivo_val ]
    then
        echo "No hay un archivo de registros validados - PROCESO CANCELADO"
        exit
    fi

#*******************************************************************************
# SCOTBATCH: c�lculo de scores
# ...
#*******************************************************************************
    echo $raya
    echo "SCOTBATCH: c�lculo de scores"
    formato="VAL"
    SCO41="041"
    SCO45="045"
    SCO47="047"
    SCO48="048"
    SCO49="049"
    SCO62="062"
    SCO67="067"
    SCO95="095"
    SCO99="099"
    scoring="$SCO41$SCO45$SCO47$SCO48$SCO49$SCO62$SCO67$SCO95$SCO99"

    echo SCOTBATCH $FECHA_PROC_YYYYMM $scoring $formato $archivo_val
    $NOHUP x SCOTBATCH $FECHA_PROC_YYYYMM $scoring $formato $archivo_val >> $archivo_log
    # DEBUG: show output >> $archivo.log 2>>$archivo.log

#*******************************************************************************
# Ejecuci�n del programa iceprerip01
# Los par�metros son:
#    archivo de entrada de validados
#    archivo de entrada de inconsistencias
#    estructura del archivo ????
#    tipo de proceso ????
#    fecha del periodo
#*******************************************************************************
     $NOHUP x iceprerip01-test $archivo_val $archivo_inc 2 A $FECHA_PROC >> $archivo_log

#*******************************************************************************
# Nombres de los archivos
#*******************************************************************************
    # muestra los nombres de los archivos del proceso
    echo $raya
    echo "ARCHIVOS:"
    echo " "
    ls $TEMPORALES/$archivo*
    echo " "

#*******************************************************************************
# Finalmente
#*******************************************************************************
    horaFin=$(date '+%H:%M:%S')
    echo " "
    echo $raya
    echo "fin del proceso "$0
    echo "Iniciado: "$horaInicio " finalizado: "$horaFin
    echo $raya
    exit

#*******************************************************************************

# El proceso batch debe incluir como mensajes de salida:
#   + Estad�sticas de Validaci�n (PESVNO)
#   + Estad�sticas de c�lculo de Score (SCOTBATCH)
#   + Nombres de los archivos generados

# Archivo Log: Por cada procedimiento se debe registrar la siguiente informaci�n:
#   + Par�metros Procesamiento
#     Archivo de par�metros
#   + Hora de inicio de procesamiento
#   + Hora de fin de procesamiento
#   + N�mero de identificaciones de entrada
#   + N�mero de registros procesados
#   + N�mero de registros no procesados
#   + Errores (en el archivo .inc)
#   +     Id procesado
#   +     Descripci�n del error
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
