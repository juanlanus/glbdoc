#sh-PE-Falabella-semestral.V.1.0001.
#**********************************************************
#SHELL-ID:     sh-PE-Falabella-semestral
#DATE-WRITTEN: 2012/11/08                                 *
#LAST UPDATE:  2012/11/08
#AUTHOR:       DIRECCION PROCESOS ESPECIALES              *
#**********************************************************
#**********************************************************

#***********************************************************************
#mostrar_parametros.                                                   *
#   Lee y muetra los parametros de ventacruzada para un suscriptor     *
#***********************************************************************
mostrar_parametros() {
  perams=`cat $cadena01.parm`
  base=`echo $perams  | cut -d ";" -f1`
  entfmt=`echo $perams  | cut -d ";" -f2`
  fecha=`echo $perams  | cut -d ";" -f3`
  echo "+-----------------------------------------------------------+"
  echo "| *** PARAMETROS PROCESO PE FALABELLA SEMESTRAL ***         |"
  echo "|       Base (I=interna, E=externa)     :  " $base
  echo "|       Formato(1=1-11,2=1-11-45)       :  " $entfmt
  echo "|       Fecha de Proceso                :  " $fecha
  echo "+-----------------------------------------------------------+"
  echo "Si desea modificar algun parametro digite S ---> \c"
  read modpard
  if test -z "$modpard"
   then
    modpar="N"
  else
    if [ $modpard == "s" ] || [ $modpard == "S" ]; then
       modpar="S"
    else
       modpar="N"
    fi
  fi
  if [ $modpar == "S" ] || [ $modpar == "s" ]; then
     modificar_parametros
  fi
}

#***********************************************************************
#pedir_parametros                                                  *
#   Pide parametros y arma el archivo PEfalsemestral
#***********************************************************************
pedir_parametros() {
  regp=""
  echo "+--------------------------------------------------+"
  echo "| La base de entrada puede ser:                    |"
  echo "|         Interna Datacredito (I)                  |"
  echo "|         o Externa           (E)                  |"
  echo "Digite tipo de base (I o E)   ---> \c"
  read base
  if test -z "$base"
   then
    base="I"
  else
   if [ $base = "E" ] || [ $base = "e" ]; then
      base="E"
   else
      base="I"
   fi
  fi
  reg=$regp$base";"
  regp=$reg
#
  echo "+--------------------------------------------------+"
  if [ $base = "E" ]; then
   echo "+--------------------------------------------------+"
   echo "|El archivo de entrada puede tener formato:        |"
   echo "|   1 - 11          (Digitar : 1)                  |"
   echo "|   1 - 11 - 45     (Digitar : 2)                  |"
   echo "Digite formato del archivo  ---> \c"
   read entfmt
   if test -z "$entfmt"
    then
     entfmt=1
   else
    if [ $entfmt -gt 2 ]; then
     entfmt=2
    else
      if [ $entfmt -lt 1 ]; then
       entfmt=1
      fi
    fi
   fi
  else
   entfmt=1
  fi
  reg=$regp$entfmt";"
  regp=$reg
  echo "+--------------------------------------------------+"
  echo "Si fecha de proceso no es la actual, digite fecha (AAAAMMDD) ---> \c"
  read fechad
  if test -z "$fechad"
   then fecha=$fechah
  else
   if [ $fechah -gt $fechad ] || [ $fechah -eq $fechad ]; then
    fecha=$fechad
   else
    echo "fecha digitada es invalida ... se asume fecha de hoy "
    fecha=$fechah
   fi
  fi
  reg=$regp$fecha";"
  regp=$reg

  fecha6=`echo $fecha  | cut -c1-6`
  echo $reg >| $cadena01.parm
  echo "Se grabo el registro de parametros"
  echo $fecha
  mostrar_parametros
#
}

#***********************************************************************
#modificar_parametros                                                  *
#   Pide los parametros a modificar y graba el archivo ventacr$codsus  *
#***********************************************************************
modificar_parametros() {
  echo "Modificar parametros"
  regp=""
  echo "+--------------------------------------------------+"
  echo "| La base de entrada puede ser:                    |"
  echo "|         Interna Datacredito (I)                  |"
  echo "|         o Externa           (E)                  |"
  echo "Digite tipo de base (I o E)   ---> \c"
  read based
  if test -z "$based"
   then
    reg=$regp$base";"
    regp=$reg
  else
   if [ $based = "E" ] || [ $based = "e" ]; then
      based="E"
   else
      based="I"
   fi
   reg=$regp$based";"
   regp=$reg
   base=$based
  fi
#
  echo "+--------------------------------------------------+"
  if [ $base = "E" ]; then
   echo "+--------------------------------------------------+"
   echo "|El archivo de entrada puede tener formato:        |"
   echo "|   1 - 11          (Digitar : 1)                  |"
   echo "|   1 - 11 - 45     (Digitar : 2)                  |"
   echo "Digite formato del archivo  ---> \c"
   read entfmtd
   if test -z "$entfmtd"
    then
     reg=$regp$entfmt";"
     regp=$reg
   else
    if [ $entfmtd -gt 3 ]; then
     entfmtd=3
    else
      if [ $entfmtd -lt 1 ]; then
       entfmtd=1
      fi
    fi
    reg=$regp$entfmtd";"
    regp=$reg
    entfmt=$entfmtd
   fi
  else
   entfmt=1
   reg=$regp$entfmt";"
   regp=$reg
  fi
  echo "+--------------------------------------------------+"
  echo "Si va a modificar fecha de proceso digite fecha (AAAAMMDD) ---> \c"
  read fechad
  if test -z "$fechad"
   then
     reg=$regp$fecha";"
     regp=$reg
  else
   if [ $fechah -gt $fechad ]; then
    fecha=$fechad
   else
    fecha=$fechah
   fi
   reg=$regp$fecha";"
   regp=$reg
  fi
  fecha6=`echo $fecha  | cut -c1-6`

  echo $reg >| $cadena01.parm
  echo "Se modifico el registro de parametros"
  mostrar_parametros
#
}

#***********************************************************************
#ejecutar_extraccion                                                   *
# extrae registros por suscriptor del archivo ICMCRECOPY.DAT           *
#***********************************************************************
ejecutar_extraccion() {
  P_SERVER_DEV="172.24.6.154"
  P_MAQUINA=`who am i | cut -c 39-50`
  echo "MAQUINA: " $P_MAQUINA
  if [ $P_MAQUINA == $P_SERVER_DEV ] ; then
       echo "MAQUINA DE DESARROLLO" $P_MAQUINA
       cd /despeciales
  else
       echo "MAQUINA DE PRODUCCION : " $P_MAQUINA
       cd $ESPECIALES/ctlc
  fi
  grep "^A......[14].*"$nitsus".P$" ICMCRECOPY.DAT | cut -c8-19 | sort -u >| $cadena01
  numeroreg1=`wc -l < $cadena01`
  mv $cadena01 $TEMPORALES/$cadena01
  entfmt=1
  cd $TEMPORALES
#
}

#PROGRAMA PRINCIPAL
nitsus=900047981
#Fecha de ejecución
fechah=`date '+%Y%m%d'`
fechah6=`echo $fechah  | cut -c1-6`
#
#*========================================================*
#*                    PROCESO                             *
#*========================================================*
echo "+--------------------------------------------------+"
echo "| **      PROCESO FALABELLA SEMESTRAL       **     |"
echo "+--------------------------------------------------+"
echo
echo "Digite nombre del archivo  ---> \c"
read cadena01
cadena00=$cadena01.prn
cadena02=$cadena01.val
cadena03=$cadena01.inc
cadena04=$cadena01.txt
cadena05=$cadena01.DATINF
cadena06=$cadena01.ESTADI
cadena07=$cadena01.QUANTO
echo
#cd $DATOS
#if test -s $cadena01.parm
# then
#  mostrar_parametros
#else
  pedir_parametros
#fi
#Fecha de Proceso
echo "Fechas: " $fechah6 " - " $fecha6
if [ $fechah6 -gt $fecha6 ]; then
#busca san_ata para la fecha historica
  cd $DATOS
  sanata=`grep $fecha6 ICSANATA.DAT | cut -c17-17`
  echo "sanata para fecha " $fecha6 "  es " $sanata
  if test -z "$sanata"
   then
   echo " !!!!   error buscando sanata para $fecha6 !!!  "
   echo "Digite san_ata para fecha historica $fecha6  ---> \c"
   read sanata
  fi
fi
cadena12=$cadena01.icgs65
cadena08=$cadena12.OK
cadena09="LOG-ICGS65-"$fecha
cadena10="LOG-ICGS65-PRB-"$fecha
cadena13=$cadena01.par
echo "Sigue el proceso .... "
if [ $base = "I" ]; then
  echo "Ejecutando extraccion   ...  "
  ejecutar_extraccion
fi
cd $TEMPORALES
if test -s $cadena01.log
 then
  rm $cadena01.log
fi
if [ $entfmt -eq 1 ]
 then
   if test -s $cadena00
    then
     rm $cadena00
   fi
   numero00=`wc -l < $cadena01`
   echo "+-------------------------------------------------------------+"
   echo "|           Registros a Procesar  --->  " $numero00
   echo "+-------------------------------------------------------+"
   nohup x CTL100 $cadena01 >| $cadena01.log
   echo "+-------------------------------------------------------+"
   echo
   tail -10 $cadena01.log
   entfmt=2
fi
echo
if [ $entfmt -eq 2 ]; then
   if test -s $cadena02
    then
     rm $cadena02
   fi
   if test -s $cadena03
    then
     rm $cadena03
   fi
 echo "+--------------------------------------------------+"
 echo "|   Ejecucion Programa PESVNO => " $cadena00
 echo "+--------------------------------------------------+"
#
 nohup x PESVNO $cadena00 $cadena02 $cadena03 10 2 >| $cadena01.log
 numero01=`wc -l < $cadena02`
 numero02=`wc -l < $cadena03`
 echo
 head -10 $cadena01.log
 echo
 echo "+-------------------------------------------------------------+"
 echo "|       ********** RESULTADOS DE VALIDACION *************     |"
 echo "|                                                             |"
 echo "|           Registros Validos    --->  " $numero01
 echo "|           Registros Invalidos  --->  " $numero02
 echo "+-------------------------------------------------------------+"
fi
if test -s $cadena02
then
   echo
   echo "Procesando archivo-----> "$cadena02
else
   echo "!!!...Archivo $cadena02 no existe proceso termina ....!!!"
   echo
   exit
fi
#
#CAMBIO DE SANATA SI SE REQUIERE
if [ $fechah6 -gt $fecha6 ]; then
 hist=/san_ata_$sanata/$fecha/extfh.cfg
 dbhist=/san_ata_$sanata/$fecha/icdb
 export EXTFH=$hist
 export DATABASE=$dbhist
 echo $EXTFH
 echo $DATABASE
fi
#
echo
mv $cadena02  $cadena04
#
#*=======================================================*
#*  EXECUTE PROGRAM: icestd81                            *
#*=======================================================*
#
echo "+----------------------------------------------+"
echo "|  Programa icestd81  DATAINFORME              |"
echo "|                                              |"
echo "|  Procesando archivo --> "  $cadena04
echo "+----------------------------------------------+"
echo
echo
nohup x icestd81 $cadena04 $cadena03 >> $cadena01.log 2>> $cadena01.log
tail -15 $cadena01.log
#
# ICGS65
clave="12345678901"
nawk -v codcla=$clave '{ printf("%s%s\n", codcla, $1) } ' $cadena04 >| $cadena12
echo $cadena12 >| $cadena13
nohup x ICGS65 BATCH $fecha < $cadena13
x ic-inp-out-sco $cadena12
cadena14=$cadena12.TODO
nawk '{ printf("%s%s%s%s%s%s%s%s%s\n",substr($0,1,1),";",substr($0,2,11),";",substr($0,13,15),";",substr($0,35,1),";",substr($0,36,50))}' $cadena12.TODO|sed -e "s/ 00000000/\;0000\;0000\;/g" >| $cadena12.ICGS65
#*========================================================*
#*       PARAMETROS CALCULO DE SCORE                      *
#*========================================================*
formato="VAL"
scoring=62
#*========================================================*
echo "+------------------------------------------------------+"
echo "|  Ejecucion Programa      SCOTBATCH                   |"
echo "+------------------------------------------------------+"
echo "|  SCORE => QUANTO                                     |"
echo "+------------------------------------------------------+"
echo
nohup x SCOTBATCH $fecha6 $scoring $formato $cadena04 >> $cadena01.log 2>>$cadena01.log
tail -12 $cadena01.log
echo
archivo=$cadena01.vec
cut -c1-12 $cadena04 >| $archivo
codsus=999999
opcion="A"
vector=48
echo "+-------------------------------------------------------+"
echo "| Ejecucion Programa  icestdatVEC-NORMAL-TOT => "
echo "+-------------------------------------------------------+"
nohup x icestdatVEC-NORMAL-FAL $archivo $fecha6 $codsus $opcion $vector >> $cadena01.log
echo
tail -12 $cadena01.log
cadena11=$archivo.$fecha6.VEC-TOT
numero11=`wc -l < $cadena11`
echo
#datainforme
echo "+----------------------------------------------------------+"
echo "|       ***  Archivos resultados Datainforme  ***          |"
echo "+----------------------------------------------------------+"
echo "|                                                          |"
echo "| Archivo Informes     => " $cadena05
echo "|                                                          |"
echo "| Archivo Estadisticas => " $cadena06
echo "+----------------------------------------------------------+"
#ICGS65
echo "+-------------------------------------------------------------+"
echo "|       ***  Archivos resultados ICGS65       ***          |"
echo "+----------------------------------------------------------+"
echo "| Archivo de salida del proceso:  " $cadena08
echo "| Archivo de salida del proceso:  " $cadena09
echo "| Archivo de salida del proceso:  " $cadena10
echo "| Archivo de salida del proceso:  " $cadena12.TODO
echo "| Archivo de salida del proceso:  " $cadena12.ICGS65
echo "+-------------------------------------------------------------+"
#QUANTO
numero07=`wc -l < $cadena07`
numero03=`wc -l < $cadena03`
echo "+-------------------------------------------------------------+"
echo "|       ***  Archivos resultados QUANTO       ***          |"
echo "+----------------------------------------------------------+"
echo "| Archivo de salida del proceso:  " $cadena07
echo "| Archivo de inconsistencias   :  " $cadena03
echo "+-------------------------------------------------------------+"
#VEC-NORMAL
echo
echo "+-------------------------------------------------------------+"
echo "|     *********  RESULTADOS DEL PROCESO VECTOR *********      |"
echo "|                                                             |"
echo "|  Archivo De salida        : " $cadena11    " Regs.:" $numero11
echo "+-------------------------------------------------------------+"
echo "        (ENTER) Continuar !!!  "
read xxx
exit

