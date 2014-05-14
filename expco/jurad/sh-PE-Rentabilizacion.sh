#sh-PE-Rentabilizacion.v.001.0003
#******************************************************************************
#ENTIDAD:      DATACREDITO                                                    *
#SHELL-ID:     sh-PE-Rentabilizacion                                          *
#DATE-WRITTEN: ENE/2013                                                       *
#AUTHOR:       DIRECCION PROCESOS ESPECIALES                                  *
#              PE Rentabilizacion de Bacno Popular                            *
#******************************************************************************
# IMS001     | CQ8909 - VERSION INICIAL - PROCESO ESPECIAL DE RENTABILIZACION *
# ENE.05-2013|          PARA BANCO POPULAR.                                   *
# M.SALAMANCA|                                                                *
#******************************************************************************
# PJV002     | CQ11513 - VERSION ESPECIAL PARA BANCO POPULAR.                 *
# ABR.10-2013| COLUMNA VALOR CUPO MÁS ALTO TDC SIN INCLUIR                    *
# PEDRO V.   | BANCO POPULAR                                                  *
#******************************************************************************
# STT003     | R18935 - SE CAMBIA ACIERTA+ POR ACIERTA A FINANCIERO           *
# ABR.01-2014|        - La variable W-TOT-CUOT suma la cuota de cooperativos  *
# LUIS GOMEZ |                                   CQ-18935                     *
#******************************************************************************
#                                                                              
#*============================================================================*
#*                           DEFINICION DE PROCESOS                           *
#*============================================================================*



GetFechaSistema()                                                              
{
  #*-----------------------------------------------------------------------------*
  # ACTUALIZA FECHA ACTUAL DEL SISTEMA
  #*-----------------------------------------------------------------------------*
  RunDate=""
  HoraHoy=`date '+%H:%M:%S'`
  FechaHoy=`date '+%Y%m%d'`
  RunDate=`echo $RunDate$FechaHoy`
  RunDate=`echo $RunDate$HoraHoy`
  PeriodoActual=`date '+%Y%m%d' | cut -c 1-6`
  AAAA_H=`echo $FechaHoy | cut -c 1-4`
  MM_H=`echo $FechaHoy | cut -c 5-6`
  DD_H=`echo $FechaHoy | cut -c 7-8`
  P_FCORTE=$FechaHoy
  echo "  |                                                         |"
  echo "       Fecha de Ejecucion    :      " $FechaHoy
  echo "       Hora de Ejecucion     :      " $HoraHoy
  echo "  |                                                         |"
  echo "  +---------------------------------------------------------+"
}




GetBaseActualoHistorica() {
  #*-----------------------------------------------------------------------------*
  # PREGUNTA SI EL PROCESO SE REALIZA CON EL ACTUAL O CON UNA BASE HISTORICA
  # EN CASO DE SER HISTORICA CAMBIA DE AMBIANTE AL SANATA CORRSPONDIENTE
  #*-----------------------------------------------------------------------------*
  echo
  echo "+-------------------------------------------------------------+"
  echo "|      **  DEFINICION DE LA BASE DATOS PARA PROCESO **        |"
  echo "+-------------------------------------------------------------+"
  echo "|          <A>   Ejecucion con BASE ACTUAL                    |"          
  echo "|          <H>   Ejecucion con BASE HISTORICA                 |"
  echo "+-------------------------------------------------------------+"
  echo "\n     Opcion  (A/H)   ---> \c" 
  read P_BASE
  PerHis=0
  if [ -z "$P_BASE" ] ; then
    PeriodoHistorico=`echo $PeriodoActual`          
    P_BASE="A"
    Periodo=1
  else
    if [ $P_BASE == "A" ] || [ $P_BASE == "a" ] ; then
      PeriodoHistorico=`echo $PeriodoActual`
      P_BASE="A"
      Periodo=1
    else
      if [ $P_BASE == "H" ] || [ $P_BASE == "h" ] ; then
        P_BASE="H"
        Periodo=2
        GetPeriodoSanata
      else
        echo "\n\t*ERRROR* OPCION NO VALIDA! <ENTER>\c"
        read ans
        P_BASE="#";
      fi
    fi
  fi
  echo "DB: " $P_BASE
}




GetPeriodoSanata()
{
  #*-----------------------------------------------------------------------------*
  # PREGUNTAR POR EL PERIODO DEL SANATA Y EFECTUAR EL CAMBIO 
  #*-----------------------------------------------------------------------------*
  echo "   "
  echo "  Digite la Fecha de la Base de Datos"
  echo "  Historica (AAAAMMDD) a Procesar  --->\c" 
  read PeriodoHistorico
  FechaProc=$PeriodoHistorico
  if [ -z "$PeriodoHistorico" ] ; then
    PeriodoHistorico=`echo $P_FANTERIOR`          
  fi
  echo "PeriodoHistorico: " $PeriodoHistorico
  longitud=`echo $PeriodoHistorico | tr -d '[:alpha:]' | awk '{printf("%d\n", length($0))}'`
  if [ $longitud -ne 8 ] && [ $longitud -ne 0 ] ; then
    echo "\t*ERROR* Periodo Historico no es válida debe ser en formato AAAAMMDD. -PROCESO CANCELADO-"
    read ans
    exit
  else
    if [ $longitud -eq 0 ] ; then
      PeriodoHistorico=`echo $PeriodoActual`
    else
      cd $DATOS
      ANOMES=`echo $PeriodoHistorico | cut -c 1-6`              
      ###          sanata=`grep $ANOMES ICSANATA.DAT | cut -c17-17`
      sanata=`grep $ANOMES ICSANATA.DAT | cut -f2 -d"/"|cut -f3 -d"_"`
      cd $TEMPORALES
      echo "Sanata para Fecha " $PeriodoHistorico "  es " $sanata
      if test -z "$sanata" ; then
        echo "\n !!!! *ERROR*  sanata para $PeriodoHistorico NO encontrada !!! \n "
        echo "                         - PROCESO CANCELADO - "
        exit
      else
        PerHis=1
        ###               CambiarSanata
      fi
    fi
  fi
}




CambiarSanata()
{
  #*-----------------------------------------------------------------------------*
  # VERIFICAR Y CAMBIAR AL SANATA CORRESPONDIENTE
  #*-----------------------------------------------------------------------------*
  ANOMES=`echo $PeriodoHistorico | cut -c 1-6`
  if [ $PeriodoActual -ne $ANOMES ] ; then
    echo "\n\tSe procesa con fecha $PeriodoHistorico \n"
    . /san_ata_$sanata/$ANOMES/sh-$ANOMES
    echo $DATABASE
    P_FCORTE=$PeriodoHistorico
  else
    P_FCORTE=$FechaCorte    
  fi
}



#
LoadParametros()
{
  #*-----------------------------------------------------------------------------*
  # LLAMAR PARAMETROS EXISTENTES O EN SU DEFECTO CREAR LOS PARAMETROS BASE                          
  #*-----------------------------------------------------------------------------*
  echo "   "
  echo " >> Cargando Parametros Para: " $NitSus
  P_CODSUS=$NitSus
  cd $DATOS
  SW_DEF=0
  ARC_PARAMETROS_DEF=PE-Rentabilizacion.par
  ARC_PARAMETROS=PE-Rentabilizacion-$P_CODSUS.par
  if [ ! -e "$ARC_PARAMETROS" ] || [ ! -f "$ARC_PARAMETROS" ] ; then

    echo "   "
    echo " >> Se cargaron Valores Por Defecto Desde: " $ARC_PARAMETROS_DEF
    cp $ARC_PARAMETROS_DEF $ARC_PARAMETROS
    SW_DEF=1
    #      SaveParametros

  fi

  while read line
  do

    LINEA=`echo "$line"`
    #      echo $LINEA
    CODIGO=`echo $LINEA | cut -c1-3`
    VALOR=`echo $LINEA | cut -c5-20`
    #      echo  " CONTENIDO LEIDO   $CODIGO   =>   $VALOR "
    case $CODIGO in
      001) 
        OpConsul=$VALOR ;;
      002) 
        text01=$VALOR ;;
      003) 
        BaseEnt=$VALOR ;;
      004) 
        text02=$VALOR ;;
      005) 
        CliFall=$VALOR ;;
      006) 
        text03=$VALOR ;;
      007) 
        AciMin=$VALOR ;;
      008) 
        MorAct=$VALOR ;;
      009) 
        MorHis=$VALOR ;;
      010) 
        MorTdc=$VALOR ;;
      011) 
        text04=$VALOR ;;
      012) 
        MorCab=$VALOR ;;
      013) 
        text05=$VALOR ;;
      014) 
        MorCop=$VALOR ;;
      015) 
        text06=$VALOR ;;
      016) 
        MorHip=$VALOR ;;
      017) 
        text07=$VALOR ;;
      018) 
        CalEnd=$VALOR ;;
      019) 
        CtaEmb=$VALOR ;;
      020) 
        CtaCmm=$VALOR ;;
      021) 
        IngMin=$VALOR ;;
      022) 
        CapPag=$VALOR ;;
      023) 
        RgEdad=$VALOR ;;
      024) 
        text08=$VALOR ;;
      025) 
        ActM30=$VALOR ;;
      026) 
        ActM60=$VALOR ;;
      027) 
        ActM90=$VALOR ;;
      028) 
        Act120=$VALOR ;;
      029) 
        ActCCt=$VALOR ;;
      030) 
        ActDRc=$VALOR ;;
      031) 
        ActCob=$VALOR ;;
      032) 
        HisM30=$VALOR ;;
      033) 
        HisM60=$VALOR ;;
      034) 
        HisM90=$VALOR ;;
      035) 
        His120=$VALOR ;;
      036) 
        HisCMM=$VALOR ;;
      037) 
        HisCRe=$VALOR ;;
    esac
  done < $ARC_PARAMETROS

  cd $TEMPORALES
}



#
SaveParametros()
{
  #*-----------------------------------------------------------------------------*
  # SALVAR PARAMETROS DEL SUSCRIPTOR                           
  #*-----------------------------------------------------------------------------*
  cd $DATOS
  ARC_PARAMETROS=PE-Rentabilizacion-$P_CODSUS.par
  echo "001=$OpConsul"      >| $ARC_PARAMETROS
  echo "002=$text01"        >> $ARC_PARAMETROS
  echo "003=$BaseEnt"       >> $ARC_PARAMETROS
  echo "004=$text02"        >> $ARC_PARAMETROS
  echo "005=$CliFall"       >> $ARC_PARAMETROS
  echo "006=$text03"        >> $ARC_PARAMETROS
  echo "007=$AciMin"        >> $ARC_PARAMETROS
  echo "008=$MorAct"        >> $ARC_PARAMETROS
  echo "009=$MorHis"        >> $ARC_PARAMETROS
  echo "010=$MorTdc"        >> $ARC_PARAMETROS
  echo "011=$text04"        >> $ARC_PARAMETROS
  echo "012=$MorCab"        >> $ARC_PARAMETROS
  echo "013=$text05"        >> $ARC_PARAMETROS
  echo "014=$MorCop"        >> $ARC_PARAMETROS
  echo "015=$text06"        >> $ARC_PARAMETROS
  echo "016=$MorHip"        >> $ARC_PARAMETROS
  echo "017=$text07"        >> $ARC_PARAMETROS
  echo "018=$CalEnd"        >> $ARC_PARAMETROS
  echo "019=$CtaEmb"        >> $ARC_PARAMETROS
  echo "020=$CtaCmm"        >> $ARC_PARAMETROS
  echo "021=$IngMin"        >> $ARC_PARAMETROS
  echo "022=$CapPag"        >> $ARC_PARAMETROS
  echo "023=$RgEdad"        >> $ARC_PARAMETROS
  echo "024=$text08"        >> $ARC_PARAMETROS
  echo "025=$ActM30"        >> $ARC_PARAMETROS
  echo "026=$ActM60"        >> $ARC_PARAMETROS
  echo "027=$ActM90"        >> $ARC_PARAMETROS
  echo "028=$Act120"        >> $ARC_PARAMETROS
  echo "029=$ActCCt"        >> $ARC_PARAMETROS
  echo "030=$ActDRc"        >> $ARC_PARAMETROS
  echo "031=$ActCob"        >> $ARC_PARAMETROS
  echo "032=$HisM30"        >> $ARC_PARAMETROS
  echo "033=$HisM60"        >> $ARC_PARAMETROS
  echo "034=$HisM90"        >> $ARC_PARAMETROS
  echo "035=$His120"        >> $ARC_PARAMETROS
  echo "036=$HisCMM"        >> $ARC_PARAMETROS
  echo "037=$HisCRe"        >> $ARC_PARAMETROS

  cd $TEMPORALES
}



#
GetEnter()
{
  #*-----------------------------------------------------------------------------*
  # SOLICITA OPRIMIR ENTER PARA CONTINUAR EJECUCION
  #*-----------------------------------------------------------------------------*
  echo
  echo "         >>>>   ( ENTER ) Continuar !!!   <<<<"
  read TECLA
}



#
GetCambiarPar()
{
  #*-----------------------------------------------------------------------------*
  # CAPTURA VALOR DE PARAMETROS A CAMBIAR
  #*-----------------------------------------------------------------------------*
  if [ $NOPCION = 2 ]
  then
    loop=0
    while [ $loop != 1 ]
    do
      clear
      echo "   "
      echo "+-------------------------------------------------------+" 
      echo "| > Opcion de Archivo de Entrada                        |"
      echo "+-------------------------------------------------------+"
      echo "  - Extraccion de DataCredito..... <I>"
      echo "  - Base Enviada Por Cliente...... <C>"
      echo "   "
      echo "   Digite su opcion ---> \c" 
      read BaseEnt                
      if test -z "$BaseEnt"
      then
        BaseEnt=I
        text02="Base DataCredito"
      fi
      if [ $BaseEnt == "I" ] || [ $BaseEnt == "i" ] ; then
        BaseEnt=I
        text02="Base DataCredito"
        loop=1
      else
        if [ $BaseEnt == "C" ] || [ $BaseEnt == "c" ] ; then
          BaseEnt=C
          text02="Base Cliente"
          loop=1
        else
          echo "  "
          echo "             !!!....Opcion Invalida....!!!"
          GetEnter
          echo
        fi
      fi
    done
  else
    if [ $NOPCION = 3 ]
    then
      loop=0
      while [ $loop != 1 ]
      do
        clear
        echo "   "
        echo "+-------------------------------------------------------+"
        echo "| > Reportados Como Fallecidos                          |"
        echo "+-------------------------------------------------------+"
        echo "  "
        echo "  - Si Aceptar....... <S>"
        echo "  - No Aceptar....... <N>"
        echo "   "
        echo "   Digite su opcion ---> \c"
        read CliFall
        if test -z "$CliFall"
        then
          CliFall=N
          text03="No Acepta"
        fi
        if [ $CliFall == "S" ] || [ $CliFall == "s" ] ; then
          CliFall=S
          text03="Si Acepta"
          loop=1
        else
          if [ $CliFall == "N" ] || [ $CliFall == "n" ] ; then
            CliFall=N
            text03="No Acepta"
            loop=1
          else
            echo "  "
            echo "             !!!....Opcion Invalida....!!!"
            GetEnter
            echo
          fi
        fi
      done
    else
      if [ $NOPCION = 4 ]
      then
        clear
        echo "   "
        echo "+-------------------------------------------------------+"
        echo "| > Puntaje de Acierta_A Minimo                         |"
        echo "+-------------------------------------------------------+"
        echo "   " 
        #CQ18935
        #    echo "  - Digite Acierta+ ---> \c"
        echo "  - Digite Acierta_A ---> \c"
        read AciMin
        if test -z "$AciMin"
        then
          AciMin=680
        fi
      else
        if [ $NOPCION = 5 ]
        then
          clear
          echo "   "
          echo "+-------------------------------------------------------+"
          echo "| > Numero de Moras Actuales de 30                      |"
          echo "+-------------------------------------------------------+"
          echo "  "
          echo "  - Digite # ---> \c"
          read ActM30
          if test -z "$ActM30"
          then
            ActM30=0
          fi
        else
          if [ $NOPCION = 6 ]
          then
            clear
            echo "   "
            echo "+-------------------------------------------------------+"
            echo "| > Numero de Moras Actuales de 60                      |"
            echo "+-------------------------------------------------------+"
            echo "  "
            echo "  - Digite # ---> \c"
            read ActM60
            if test -z "$ActM60"
            then
              ActM60=0
            fi
          else

            if [ $NOPCION = 7 ]
            then
              clear
              echo "   "
              echo "+-------------------------------------------------------+"
              echo "| > Numero de Moras Actuales de 90                      |"
              echo "+-------------------------------------------------------+"
              echo "  "
              echo "  - Digite # ---> \c"
              read ActM90
              if test -z "$ActM90"
              then
                ActM90=0
              fi
            else
              if [ $NOPCION = 8 ]
              then
                clear
                echo "   "
                echo "+-------------------------------------------------------+"
                echo "| > Numero de Moras Actuales de 120                     |"
                echo "+-------------------------------------------------------+"
                echo "  "
                echo "  - Digite # ---> \c"
                read Act120
                if test -z "$Act120"
                then
                  Act120=0
                fi
              else    
                if [ $NOPCION = 9 ]
                then
                  clear
                  echo "   "
                  echo "+-------------------------------------------------------+"
                  echo "| > Numero de Carteras Castigadas Actuales              |"
                  echo "+-------------------------------------------------------+"
                  echo "  "
                  echo "  - Digite # ---> \c"
                  read ActCCt
                  if test -z "$ActCCt"
                  then
                    ActCCt=0
                  fi
                else
                  if [ $NOPCION = 10 ]
                  then
                    clear
                    echo "   "
                    echo "+-------------------------------------------------------+"
                    echo "| > Numero de Dudosos Recaudos Actuales                 |"
                    echo "+-------------------------------------------------------+"
                    echo "  "
                    echo "  - Digite # ---> \c"
                    read ActDRc
                    if test -z "$ActDRc"
                    then
                      ActDRc=0
                    fi
                  else
                    if [ $NOPCION = 11 ]
                    then
                      clear
                      echo "   "
                      echo "+-------------------------------------------------------+"
                      echo "| > Numero En Cobrador Actuales                         |"
                      echo "+-------------------------------------------------------+"
                      echo "  "
                      echo "  - Digite # ---> \c"
                      read ActCob
                      if test -z "$ActCob"
                      then
                        ActCob=0
                      fi
                    else
                      if [ $NOPCION = 12 ]
                      then
                        clear
                        echo "   "
                        echo "+-------------------------------------------------------+"
                        echo "| > Numero de Moras Historicas de 30                    |"
                        echo "+-------------------------------------------------------+"
                        echo "  "
                        echo "  - Digite # ---> \c"
                        read HisM30
                        if test -z "$HisM30"
                        then
                          HisM30=0
                        fi
                      else
                        if [ $NOPCION = 13 ]
                        then
                          clear
                          echo "   "
                          echo "+-------------------------------------------------------+"
                          echo "| > Numero de Moras Historicas de 60                    |"
                          echo "+-------------------------------------------------------+"
                          echo "  "
                          echo "  - Digite # ---> \c"
                          read HisM60
                          if test -z "$HisM60"
                          then
                            HisM60=0
                          fi
                        else
                          if [ $NOPCION = 14 ]
                          then
                            clear
                            echo "   "
                            echo "+-------------------------------------------------------+"
                            echo "| > Numero de Moras Historicas de 90                    |"
                            echo "+-------------------------------------------------------+"
                            echo "  "
                            echo "  - Digite # ---> \c"
                            read HisM90
                            if test -z "$HisM90"
                            then
                              HisM90=0
                            fi
                          else
                            if [ $NOPCION = 15 ]
                            then
                              clear
                              echo "   "
                              echo "+-------------------------------------------------------+"
                              echo "| > Numero de Moras Historicas de 120                   |"
                              echo "+-------------------------------------------------------+"
                              echo "  "
                              echo "  - Digite # ---> \c"
                              read His120
                              if test -z "$His120"
                              then
                                His120=0
                              fi
                            else
                              if [ $NOPCION = 16 ]
                              then
                                clear
                                echo "   "
                                echo "+-------------------------------------------------------+"
                                echo "| > Numero de Canceladas Mal Manejo Historicas          |"
                                echo "+-------------------------------------------------------+"
                                echo "  "
                                echo "  - Digite # ---> \c"
                                read HisCMM
                                if test -z "$HisCMM"
                                then
                                  HisCMM=0
                                fi
                              else
                                if [ $NOPCION = 17 ]
                                then
                                  clear
                                  echo "   "
                                  echo "+-------------------------------------------------------+"
                                  echo "| > Numero de Carteras Recuperadas Historicas           |"
                                  echo "+-------------------------------------------------------+"
                                  echo "  "
                                  echo "  - Digite # ---> \c"
                                  read HisCRe
                                  if test -z "$HisCRe"
                                  then
                                    HisCRe=0
                                  fi
                                else
                                  if [ $NOPCION = 18 ]
                                  then
                                    loop=0
                                    while [ $loop != 1 ]
                                    do
                                      clear
                                      echo "   "
                                      echo "+-------------------------------------------------------+"
                                      echo "| > Altura Maxima Moras TDC                             |"
                                      echo "+-------------------------------------------------------+"
                                      echo "  "
                                      echo "  - Mora de 0........ <0>"
                                      echo "  - Mora de 30....... <1>"
                                      echo "  - Mora de 60....... <2>"
                                      echo "  - Mora de 90....... <3>"
                                      echo "  - Mora de 120...... <4>"
                                      echo "   "
                                      echo "   Digite su opcion ---> \c"
                                      read MorTdc
                                      if test -z "$MorTdc"
                                      then
                                        MorTdc=0
                                        text04="Mora de 0"
                                      fi
                                      if [ $MorTdc == "0" ] ; then 
                                        text04="Mora de 0"
                                        loop=1
                                      else
                                        if [ $MorTdc == "1" ] ; then
                                          text04="Mora de 30"
                                          loop=1
                                        else
                                          if [ $MorTdc == "2" ] ; then
                                            text04="Mora de 60"
                                            loop=1
                                          else
                                            if [ $MorTdc == "3" ] ; then
                                              text04="Mora de 90"
                                              loop=1
                                            else
                                              if [ $MorTdc == "4" ] ; then  
                                                text04="Mora de 120"
                                                loop=1
                                              else
                                                echo "  "
                                                echo "             !!!....Opcion Invalida....!!!"
                                                GetEnter
                                                echo
                                              fi
                                            fi
                                          fi
                                        fi
                                      fi
                                    done
                                  else
                                    if [ $NOPCION = 19 ]
                                    then
                                      loop=0
                                      while [ $loop != 1 ]
                                      do
                                        clear
                                        echo "   "
                                        echo "+-------------------------------------------------------+"
                                        echo "| > Altura Maxima Moras CAB                             |"
                                        echo "+-------------------------------------------------------+"
                                        echo "  "
                                        echo "  - Mora de 0........ <0>"
                                        echo "  - Mora de 30....... <1>"
                                        echo "  - Mora de 60....... <2>"
                                        echo "  - Mora de 90....... <3>"
                                        echo "  - Mora de 120...... <4>"
                                        echo "   "
                                        echo "   Digite su opcion ---> \c"
                                        read MorCab
                                        if test -z "$MorCab"
                                        then
                                          MorCab=0
                                          text05="Mora de 0"
                                        fi
                                        if [ $MorCab == "0" ] ; then
                                          text05="Mora de 0"
                                          loop=1
                                        else
                                          if [ $MorCab == "1" ] ; then
                                            text05="Mora de 30"
                                            loop=1
                                          else
                                            if [ $MorCab == "2" ] ; then
                                              text05="Mora de 60"
                                              loop=1
                                            else
                                              if [ $MorCab == "3" ] ; then
                                                text05="Mora de 90"
                                                loop=1
                                              else
                                                if [ $MorCab == "4" ] ; then
                                                  text05="Mora de 120"
                                                  loop=1
                                                else
                                                  echo "  "
                                                  echo "             !!!....Opcion Invalida....!!!"
                                                  GetEnter
                                                  echo
                                                fi
                                              fi
                                            fi
                                          fi
                                        fi
                                      done
                                    else
                                      if [ $NOPCION = 20 ]
                                      then
                                        loop=0
                                        while [ $loop != 1 ]
                                        do
                                          clear
                                          echo "   "
                                          echo "+-------------------------------------------------------+"
                                          echo "| > Altura Maxima Moras Cooperativo                     |"
                                          echo "+-------------------------------------------------------+"
                                          echo "  "
                                          echo "  - Mora de 0........ <0>"
                                          echo "  - Mora de 30....... <1>"
                                          echo "  - Mora de 60....... <2>"
                                          echo "  - Mora de 90....... <3>"
                                          echo "  - Mora de 120...... <4>"
                                          echo "   "
                                          echo "   Digite su opcion ---> \c"
                                          read MorCop
                                          if test -z "$MorCop"
                                          then
                                            MorCop=0
                                            text06="Mora de 0"
                                          fi
                                          if [ $MorCop == "0" ] ; then
                                            text06="Mora de 0"
                                            loop=1
                                          else
                                            if [ $MorCop == "1" ] ; then
                                              text06="Mora de 30"
                                              loop=1
                                            else
                                              if [ $MorCop == "2" ] ; then
                                                text06="Mora de 60"
                                                loop=1
                                              else
                                                if [ $MorCop == "3" ] ; then
                                                  text06="Mora de 90"
                                                  loop=1
                                                else
                                                  if [ $MorCop == "4" ] ; then
                                                    text06="Mora de 120"
                                                    loop=1
                                                  else
                                                    echo "  "
                                                    echo "             !!!....Opcion Invalida....!!!"
                                                    GetEnter
                                                    echo
                                                  fi
                                                fi
                                              fi
                                            fi
                                          fi
                                        done
                                      else
                                        if [ $NOPCION = 21 ]
                                        then
                                          loop=0
                                          while [ $loop != 1 ]
                                          do
                                            clear
                                            echo "   "
                                            echo "+-------------------------------------------------------+"
                                            echo "| > Altura Maxima Moras Hipotecario                     |"
                                            echo "+-------------------------------------------------------+"
                                            echo "  "
                                            echo "  - Mora de 0........ <0>"
                                            echo "  - Mora de 30....... <1>"
                                            echo "  - Mora de 60....... <2>"
                                            echo "  - Mora de 90....... <3>"
                                            echo "  - Mora de 120...... <4>"
                                            echo "   "
                                            echo "   Digite su opcion ---> \c"
                                            read MorHip
                                            if test -z "$MorHip"
                                            then
                                              MorHip=0
                                              text07="Mora de 0"
                                            fi
                                            if [ $MorHip == "0" ] ; then
                                              text07="Mora de 0"
                                              loop=1
                                            else
                                              if [ $MorHip == "1" ] ; then
                                                text07="Mora de 30"
                                                loop=1
                                              else
                                                if [ $MorHip == "2" ] ; then
                                                  text07="Mora de 60"
                                                  loop=1
                                                else
                                                  if [ $MorHip == "3" ] ; then
                                                    text07="Mora de 90"
                                                    loop=1
                                                  else
                                                    if [ $MorHip == "4" ] ; then
                                                      text07="Mora de 120"
                                                      loop=1
                                                    else
                                                      echo "  "
                                                      echo "             !!!....Opcion Invalida....!!!"
                                                      GetEnter
                                                      echo
                                                    fi
                                                  fi
                                                fi
                                              fi
                                            fi
                                          done
                                        else
                                          if [ $NOPCION = 22 ]
                                          then
                                            loop=0
                                            while [ $loop != 1 ]
                                            do
                                              clear
                                              echo "   "
                                              echo "+-------------------------------------------------------+"
                                              echo "| > Calificacion de Endeudamiento                       |"
                                              echo "+-------------------------------------------------------+"
                                              echo "  "
                                              echo "  - Calificacion A...... <A>"
                                              echo "  - Calificacion B...... <B>"
                                              echo "  - Calificacion C...... <C>"
                                              echo "  - Calificacion D...... <D>"
                                              echo "  - Calificacion E...... <E>"
                                              echo "   "
                                              echo "   Digite su opcion ---> \c"
                                              read CalEnd
                                              if test -z "$CalEnd"
                                              then
                                                CalEnd=A
                                              fi
                                              if [ $CalEnd == "A" ] || [ $CalEnd == "a" ] ; then
                                                CalEnd=A
                                                loop=1
                                              else                
                                                if [ $CalEnd == "B" ] || [ $CalEnd == "b" ] ; then
                                                  CalEnd=B
                                                  loop=1
                                                else
                                                  if [ $CalEnd == "C" ] || [ $CalEnd == "c" ] ; then
                                                    CalEnd=C
                                                    loop=1
                                                  else
                                                    if [ $CalEnd == "D" ] || [ $CalEnd == "d" ] ; then
                                                      CalEnd=D
                                                      loop=1
                                                    else
                                                      if [ $CalEnd == "E" ] || [ $CalEnd == "e" ] ; then  
                                                        CalEnd=E
                                                        loop=1
                                                      else
                                                        echo "  "
                                                        echo "             !!!....Opcion Invalida....!!!"
                                                        GetEnter
                                                        echo
                                                      fi
                                                    fi
                                                  fi
                                                fi
                                              fi
                                            done
                                          else
                                            if [ $NOPCION = 23 ]
                                            then
                                              clear
                                              echo "   "
                                              echo "+-------------------------------------------------------+"
                                              echo "| > Numero de Cuentas Embargadas                        |"
                                              echo "+-------------------------------------------------------+"
                                              echo "  "
                                              echo "  - Digite # ---> \c"
                                              read CtaEmb
                                              if test -z "$CtaEmb"
                                              then
                                                CtaEmb=0
                                              fi
                                            else
                                              if [ $NOPCION = 24 ]
                                              then
                                                clear
                                                echo "   "
                                                echo "+-------------------------------------------------------+"
                                                echo "| > Numero de Cuentas Canceladas Mal Manejo             |"
                                                echo "+-------------------------------------------------------+"
                                                echo "  "
                                                echo "  - Digite # ---> \c"
                                                read CtaCmm
                                                if test -z "$CtaCmm"
                                                then
                                                  CtaCmm=0
                                                fi
                                              else
                                                if [ $NOPCION = 25 ]
                                                then
                                                  clear
                                                  echo "   "
                                                  echo "+-------------------------------------------------------+"
                                                  echo "| > Ingresos Minimos - Numero de Salarios Minimos       |"
                                                  echo "+-------------------------------------------------------+"
                                                  echo "  "
                                                  echo "  - Digite # ---> \c"
                                                  read IngMin
                                                  if test -z "$IngMin"
                                                  then
                                                    IngMin=2.5
                                                  fi
                                                else
                                                  if [ $NOPCION = 26 ]
                                                  then              
                                                    clear
                                                    echo "   "
                                                    echo "+-------------------------------------------------------+"
                                                    echo "| > Porcentaje Minimo de Capacidad de Pago              |"
                                                    echo "+-------------------------------------------------------+"
                                                    echo "  "
                                                    echo "  - Digite % ---> \c"
                                                    read CapPag
                                                    if test -z "$CapPag"
                                                    then
                                                      CapPag=50
                                                    fi
                                                  else
                                                    if [ $NOPCION = 27 ]
                                                    then
                                                      loop=0
                                                      while [ $loop != 1 ]
                                                      do
                                                        clear
                                                        echo "   "
                                                        echo "+-------------------------------------------------------+"
                                                        echo "| > Rango de Edad                                       |"
                                                        echo "+-------------------------------------------------------+"
                                                        echo "  "
                                                        echo "  - 18-21...... <1>"
                                                        echo "  - 22-28...... <2>"
                                                        echo "  - 29-35...... <3>"
                                                        echo "  - 36-45...... <4>"
                                                        echo "  - 46-55...... <5>"
                                                        echo "  - 56-65...... <6>"
                                                        echo "  - 66+........ <7>"
                                                        echo "   "
                                                        echo "   Digite su opcion ---> \c"
                                                        read RgEdad
                                                        if test -z "$RgEdad"
                                                        then
                                                          RgEdad=7
                                                          text08="66+"
                                                        fi
                                                        if [ $RgEdad == "1" ] ; then
                                                          text08="18-21"
                                                          loop=1
                                                        else
                                                          if [ $RgEdad == "2" ] ; then
                                                            text08="22-28"
                                                            loop=1
                                                          else
                                                            if [ $RgEdad == "3" ] ; then
                                                              text08="29-35"
                                                              loop=1
                                                            else
                                                              if [ $RgEdad == "4" ] ; then
                                                                text08="36-45"
                                                                loop=1
                                                              else
                                                                if [ $RgEdad == "5" ] ; then  
                                                                  text08="46-55"
                                                                  loop=1
                                                                else
                                                                  if [ $RgEdad == "6" ] ; then
                                                                    text08="56-65"
                                                                    loop=1
                                                                  else
                                                                    if [ $RgEdad == "7" ] ; then
                                                                      text08="66+"
                                                                      loop=1
                                                                    else
                                                                      echo "  "
                                                                      echo "             !!!....Opcion Invalida....!!!"
                                                                      GetEnter
                                                                      echo
                                                                    fi
                                                                  fi
                                                                fi
                                                              fi
                                                            fi
                                                          fi
                                                        fi
                                                      done
                                                    fi
                                                  fi
                                                fi
                                              fi
                                            fi
                                          fi
                                        fi
                                      fi
                                    fi
                                  fi
                                fi
                              fi
                            fi
                          fi
                        fi
                      fi
                    fi
                  fi
                fi
              fi
            fi
          fi
        fi
      fi
    fi
  fi
}




GetSuscriptorNit()
{
  #*-----------------------------------------------------------------------------*
  # SOLICITA SUSCRIPTOR / NIT Y EL NUMERO
  #*-----------------------------------------------------------------------------*
  loop=0
  while [ $loop != 1 ]
  do
    clear
    echo "   "
    echo " +-------------------------------------------------------+"
    echo " |  - Para realizar la consulta por NIT........ <ENTER>  |"
    echo " |  - Para cambiar a SUSCRIPTOR.......... <S> + <ENTER>  |"
    echo " +-------------------------------------------------------+"
    echo "    Digite su Opcion ---> \c"
    read OpConsul
    if test -z "$OpConsul"
    then
      OpConsul=N
      text01="NIT"
      echo "   "
      echo "    Digite el NIT a consultar : \c"
      read NitSus
      loop=1
    else 
      if [ $OpConsul == "S" ] || [ $OpConsul == "s" ] ; then
        OpConsul=S
        text01="Suscriptor"
        echo "   "
        echo "    Digite el SUSCRIPTOR a consultar : \c"
        read NitSus
        loop=1
      else
        echo "   "
        echo "             !!!....Opcion Invalida....!!!"
        GetEnter
        echo
      fi    
    fi
  done
}




GetArchivo()
{
  #*-----------------------------------------------------------------------------*
  # RECIBE ARCHIVO DE ENTRADA
  #*-----------------------------------------------------------------------------*
  clear
  if [ $BaseEnt == "C" ] ; then
    cd $TEMPORALES
    echo "   "
    echo "+-------------------------------------------------------+"
    echo "|  > Digite Nombre Del Archivo De Entrada Sin Extencion |"
    echo "+-------------------------------------------------------+"
    echo "    Archivo ---> \c"
    read NomArch
    SetArchivos
    if test -s $cadena01
    then
      echo
      echo "  - Archivo Ubicado --->  " $cadena01
      echo
    else
      echo "!!!...Archivo $cadena01 no existe proceso termina ....!!!"
      echo
      exit
    fi
    loop=0
    while [ $loop != 1 ]
    do
      clear
      echo "   "
      echo "+-------------------------------------------------------+"
      echo "| > Formato de Archivo de Entrada                       |"
      echo "+-------------------------------------------------------+"
      echo "  "
      echo "  - 1-11 .......... <1>"
      echo "  - 1-11-45 ....... <2>"
      echo "   "
      echo "   Digite su opcion ---> \c"
      echo "  "
      read FmtArc
      if test -z "$FmtArc"
      then
        FmtArc=1
      fi
      if [ $FmtArc == "1" ] ; then
        echo "  - Estructura Archivo 1-11 (TipoID-NumID) "
        cp $cadena01 $cadena03
        echo
        echo " +-------------------------------------------------------+"
        echo " |  > Clasificacion Unica  SORT +0.0b -0.12b -u          |"
        echo " +-------------------------------------------------------+"
        sort +0.0b -0.12b -u $cadena03 -o $cadena01
        echo
        echo "  - PESVNO - Asume Funcion de Nombres"
        cat $DATOS/REGPESVNO.VALI $cadena01 >| $cadena04
        ###       wc -l $cadena01 $cadena01.cat
        cp $cadena04 $cadena01
        rm $cadena03 $cadena04
        loop=1
      else
        if [ $FmtArc == "2" ] ; then
          echo "  - Estructura Archivo 1-11-45 (TipoID-NumID-Nombre) "
          loop=1
        else
          echo "  "
          echo "             !!!....Opcion Invalida....!!!"
          GetEnter
          echo
        fi
      fi
    done
  else
    echo "   "
    echo " +-------------------------------------------------------+"
    echo " |  > Realizando extraccion de ICMCRECOPY                |"
    echo " +-------------------------------------------------------+"
    NomArch=$NitSus-ExtDC-$FechaProc
    SetArchivos
    ambiente=`echo $SERV_CCI | cut -c 1-5`
    if [ $ambiente == "desno" ] || [ $ambiente == "DES" ] || [ $ambiente == "pruno" ] || [ $ambiente == "pro-4" ]; then
      ###echo " Para Desarrollo "
      cd $TEMPORALES
      grep $NitSus ICMCRECOPY.DAT >| $cadena01
    else
      ###echo " Para Produccion "
      cd /especiales/ctlc
      grep $NitSus ICMCRECOPY.DAT >| $cadena01
      mv $cadena01 $TEMPORALES
      cd $TEMPORALES
    fi
    #
    grep ^A $cadena01 >| $cadena02
    #  
    cut -c8-19  $cadena02 >| $cadena03
    #  
    echo
    echo " +-------------------------------------------------------+"
    echo " |  > Clasificacion Unica  SORT +0.0b -0.12b -u          |"
    echo " +-------------------------------------------------------+"
    sort +0.0b -0.12b -u $cadena03 -o $cadena01
    echo
    echo "   - PESVNO - Asume Funcion de Nombres"
    cat $DATOS/REGPESVNO.VALI $cadena01 >| $cadena04
    ###wc -l $cadena01 $cadena04
    cp $cadena04 $cadena01
    rm $cadena02 $cadena03 $cadena04
    echo
  fi
}



#
SetArchivos()
{
  #*-----------------------------------------------------------------------------*
  # SE DA NOMBRE A LOS ARCHIVOS A USAR
  #*-----------------------------------------------------------------------------*
  #
  cadena01=$NomArch.prn
  cadena02=$cadena01.OK
  cadena03=$cadena01.ACTV
  cadena04=$NomArch.cat
  cadena05=$NomArch.val
  cadena06=$NomArch.inc
  cadena07=$NomArch.txt
  cadena08=$NomArch.DATINF
  cadena09=$NomArch.ESTADI
  #CQ-18935: se cambia ACIERTA+ por ACIERTA A FINANCIERO
  #cadena10=$NomArch.PREDHD
  cadena10=$NomArch.AADFGE 
  #CQ-18935                                               
  cadena11=$NomArch.QUANTO
  cadena12=$NomArch.ULTIMO
  cadena13=$NomArch.SI-RENT
  cadena14=$NomArch.NO-RENT
  cadena15=$NomArch.val2
  cadena16=$NomArch.CON
  cadena17=$NomArch.SIN
  cadena18=$NomArch.TOT
  logproc=$NomArch.log
  #
}




EjecutaProceso()
{
  #*-----------------------------------------------------------------------------*
  # EJECUTA PROCESO (PESVNO-DATAINFORME-SCOTBATCH-INTEGRACION-RECONOCER)
  #*-----------------------------------------------------------------------------*
  #*============================================================================*
  #*  EXECUTE PROGRAM: PESVNO                                                   *
  #*============================================================================*
  echo "                 "
  echo "+-------------------------------------------------------------+"
  echo "|         V A L I D A C I O N   D E   N O M B R E S           |" 
  echo "+-------------------------------------------------------------+"
  echo "|                                                             |"
  echo "   > Validando Archivo --> " $cadena01
  echo "  "
  nohup x PESVNO $cadena01 $cadena05 $cadena06 10 2 >| $logproc 2>> $logproc
  echo "  "
  grep Registros $logproc
  echo "  "
  echo "   > Finalizo Paso de Validacion!!!"
  #
  numero01=`wc -l < $cadena01`
  numero02=`wc -l < $cadena05`
  numero03=`wc -l < $cadena06`
  echo "|                                                             |"
  echo "+-------------------------------------------------------------+"
  echo "  |          ***   RESULTADOS DE VALIDACION   ***           |"
  echo "  +---------------------------------------------------------+"
  echo "  |                                                         |"
  echo "      - Archivo Validado        : " $cadena05
  echo "      - Archivo Inconsistencias : " $cadena06
  echo "  |                                                         |"
  echo "  |---------------------------------------------------------|"
  echo "  |                                                         |"
  echo "      - Registros Validos     -->  " $numero02
  echo "      - Registros Incorrectos -->  " $numero03
  echo "      - Registros Procesados  -->  " $numero01
  echo "  |                                                         |"
  echo "  +---------------------------------------------------------+"
  echo "  "
  #
  cp $cadena05 $cadena07
  #
  #*============================================================================*
  #*  SI ES PERIODO HISTORICO, REALIZA EL CAMBIO DE SANATA
  #*============================================================================*
  if [ $PerHis == "1" ] ; then
    CambiarSanata
    echo "  "
    echo "+-------------------------------------------------------------+"
    echo "|                                                             |"
    echo "    > Cambio de SANATA Realizado!!! - Periodo: " $PeriodoHistorico
    echo "|                                                             |"
    echo "+-------------------------------------------------------------+"
    echo "  "
  fi
  #
  #*============================================================================*
  #*  EXECUTE PROGRAM: icestd81                                                 *
  #*============================================================================*
  echo "                 "
  echo "+-------------------------------------------------------------+"
  echo "|                  D A T A   I N F O R M E                    |"
  echo "+-------------------------------------------------------------+"
  echo "|                                                             |"
  echo "   > Procesando Archivo --> " $cadena07
  echo "  "
  nohup x icestd81bp1 $cadena07 $cadena06 >> $logproc 2>> $logproc
  tail -20 $logproc
  echo "  "
  echo "   > Termino Proceso de Datainforme!!!"
  #
  echo "|                                                             |"
  echo "+-------------------------------------------------------------+"
  echo "  |       ***  ARCHIVOS RESULTADOS DE CONSULTA  ***         |"
  echo "  +---------------------------------------------------------+"
  echo "  |                                                         |"
  echo "     - Archivo Informes     --> " $cadena08
  echo "     - Archivo Estadisticas --> " $cadena09
  echo "  |                                                         |"
  echo "  +---------------------------------------------------------+"
  echo "  "
  #*============================================================================*
  #*  EXECUTE PROGRAM: SCOTBATCH                                                *
  #*============================================================================*
  formato="VAL"
  # CQ-18935: SE CAMBIA ACIERTA+ POR ACIERTA A FINANCIERO
  #scoring=6762
  scoring=4762
  # CQ-18935
  ANOMES=`echo $FechaProc | cut -c 1-6`
  echo "                 "
  echo "+-------------------------------------------------------------+"
  echo "|            A C I E R T A  A   -   Q U A N T O               |"
  echo "+-------------------------------------------------------------+"
  echo "|                                                             |"
  echo "   > Calculando Acierta_A y Quanto"
  echo "  "
  nohup x SCOTBATCH $ANOMES $scoring $formato $cadena05 >>$logproc 2>> $logproc
  tail -12 $logproc
  echo "  "
  echo "   > Termino Calculo de Acierta_A y Quanto!!!"
  echo "|                                                             |"
  echo "+-------------------------------------------------------------+"
  echo "                 "
  #*============================================================================*
  #*  EXECUTE PROGRAM: iceinteg0611.cbl                                         *
  #*============================================================================*
  echo "                 "
  echo "+-------------------------------------------------------------+"
  echo "|                R E N T A B I L I Z A C I O N                |"
  echo "+-------------------------------------------------------------+"
  echo "|                                                             |"
  echo "   > Realizando Rentabilizacion de Clientes"
  echo "  "
  nohup x iceinteg0611 $cadena05 $cadena11 $cadena10 $cadena08 $cadena13 $cadena14 $cadena15 $CliFall $AciMin $MorTdc $MorCab $MorCop $MorHip $CalEnd $CtaEmb $CtaCmm $IngMin $CapPag $RgEdad $ActM30 $ActM60 $ActM90 $Act120 $ActCCt $ActDRc $ActCob $HisM30 $HisM60 $HisM90 $His120 $HisCMM $HisCRe >> $logproc 2>> $logproc
  tail -12 $logproc
  echo "  "
  echo "   > Termino Rentabilizacion!!!"
  echo "|                                                             |"
  echo "+-------------------------------------------------------------+"
  echo "  |         ***  ARCHIVOS DE RENTABILIZACION  ***           |"
  echo "  +---------------------------------------------------------+"
  echo "  |                                                         |"
  echo "     - SI Rentabilizables --> " $cadena13
  echo "     - NO Rentabilizables --> " $cadena14
  echo "  |                                                         |"
  echo "  +---------------------------------------------------------+"
  echo "                 "
  #*============================================================================*
  #*  EXECUTE PROGRAM: icedir11                                                 *
  #*============================================================================*
  numero04=`wc -l < $cadena05`
  Suscriptor=980001
  OpcProc=2
  sal1="SI"
  sal2="SI"
  sal3="SI"
  sal4="SI"
  echo "                 "
  echo "+-------------------------------------------------------------+"
  echo "|            R E C O N O C E R +   S I M P L E                |"
  echo "+-------------------------------------------------------------+"
  echo "|                                                             |"
  echo "   > Procesando Archivo Unificado"
  echo "  "
  nohup x icedir11 $Suscriptor $cadena15 $OpcProc $numero04 $sal1$sal2$sal3$sal4  >>$logproc 2>> $logproc
  tail -18 $logproc
  echo "  "
  echo "Formatea Archivo Con Direcciones: " $cadena16
  cp $cadena16 $cadena16-form
  wc -l $cadena16 $cadena16-form
  nohup x iceinteg0611-f $cadena16-form >> $logproc 2>> $logproc
  echo "   > Termino Validacion de Reconocer!!!"
  echo "|                                                             |"
  echo "+-------------------------------------------------------------+"
  echo "  |          ***   RESULTADOS DE RECONOCER   ***            |"
  echo "  +---------------------------------------------------------+"
  echo "  |                                                         |"
  echo "     - Archivo CON Direcciones: " $cadena16
  echo "     - Archivo SIN Direcciones: " $cadena17
  echo "     - Archivo TOT Estadistica: " $cadena18
  echo "  |                                                         |"
  echo "  +---------------------------------------------------------+"
  echo "  "
  echo "  "
  echo "  -----------------------------------------------------------"
  echo "+----------                                         ----------+"
  echo "| -------  sh-PE-Rentabilizacion Termino Proceso!!!!  ------- |"
  echo "+----------                                         ----------+"
  echo "  -----------------------------------------------------------"
}



####
GetMuestraVariables()
{
  clear
  echo "   "
  echo "   "
  echo "   "
  echo " +-------------------------------------------------------+"
  echo " |        ***   PARAMETROS PARA EJECUCION     ***        |"
  echo " +-------------------------------------------------------+"
  echo "     Fecha Actual              : " $FechaHoy
  echo "     Fecha Base de Ejecucion   : " $FechaProc
  echo " +-------------------------------------------------------+"
  echo " |                                                       |"
  echo "      1- Consultar Por:         : " $text01
  echo "                                : " $NitSus
  echo "      2- Base de Entrada        : " $text02
  echo "      3- Acepta Fallecidos      : " $text03
  echo "      4- Acierta_A Minimo       : " $AciMin
  echo "      5- Act. Mora de 30        : " $ActM30
  echo "      6- Act. Mora de 60        : " $ActM60
  echo "      7- Act. Mora de 90        : " $ActM90
  echo "      8- Act. Mora de 120       : " $Act120
  echo "      9- Act. Cartera Castigada : " $ActCCt
  echo "     10- Act. Dudoso Recaudo    : " $ActDRc
  echo "     11- Act. En Cobrador       : " $ActCob
  echo "     12- His. Mora de 30        : " $HisM30
  echo "     13- His. Mora de 60        : " $HisM60
  echo "     14- His. Mora de 90        : " $HisM90
  echo "     15- His. Mora de 120       : " $His120
  echo "     16- His. Mal Manejo        : " $HisCMM
  echo "     17- His. Cart. Recuperada  : " $HisCRe
  echo "     18- Altura Mora TDC        : " $text04
  echo "     19- Altura Mora CAB        : " $text05
  echo "     20- Altura Mora Cooper     : " $text06
  echo "     21- Altura Mora Hipote     : " $text07
  echo "     22- Calif. Endeudamiento   : " $CalEnd
  echo "     23- Ctas Embargadas        : " $CtaEmb
  echo "     24- Ctas Cancel Mal Manej  : " $CtaCmm
  echo "     25- Ingresos Minimos       : " $IngMin " Salarios Minimos"
  echo "     26- Capacidad de Pago      : " $CapPag "%"
  echo "     27- Rango de Edad          : " $text08
  echo " |                                                       |"
  echo " +-------------------------------------------------------+"  
}

#*============================================================================*
#*                                 PROCESO                                    *
#*============================================================================*
clear
echo "+-------------------------------------------------------------+"
echo "| ------------   R E N T A B I L I Z A C I O N   ------------ |"
echo "| --------------   B A N C O   P O P U L A R   -------------- |"
echo "+-------------------------------------------------------------+"
GetFechaSistema
FechaProc=$FechaHoy
GetBaseActualoHistorica
GetSuscriptorNit
LoadParametros
#
LoopMenu=0
OPCION=-1
while [ $LoopMenu != 1 ] ; do
  NOPCION=0
  clear
  GetMuestraVariables
  echo "   |  - Si los parametros de ejecucion son correctos:  |"
  echo "   |           Digite.................. <0> + <ENTER>  |"
  echo "   |  - Si desea cambiar algun parametro:              |"
  echo "   |           Digite...... Numero Asociado + <ENTER>  |"
  echo "   +---------------------------------------------------+"
  echo "   "
  echo "       Digite Su Opcion ---> \c"
  read NOPCION
  if test -z "$NOPCION" ; then 
    LoopMenu=0
  else

    case $NOPCION in
      0)                                        
        LoopMenu=1
        ;;
      1)
        ### Suscriptor / NIT a revisar
        GetSuscriptorNit
        ;;
      2)
        ### Base de Entrada
        GetCambiarPar
        ;;              
      3)
        ### Fallecidos
        GetCambiarPar
        ;;              
      4)
        ### Acierta_A
        GetCambiarPar
        ;;              
      5)
        ### Act. Mora de 30
        GetCambiarPar
        ;;                            
      6)   
        ### Act. Mora de 60
        GetCambiarPar
        ;;
      7)   
        ### Act. Mora de 90
        GetCambiarPar
        ;;
      8)   
        ### Act. Mora de 120
        GetCambiarPar
        ;;
      9)   
        ### Act. Cartera Castigada
        GetCambiarPar
        ;;
      10)   
        ### Act. Dudoso Recaudo
        GetCambiarPar
        ;;
      11)   
        ### Act. En Cobrador
        GetCambiarPar
        ;;
      12)   
        ### His. Mora de 30
        GetCambiarPar
        ;;
      13)   
        ### His. Mora de 60
        GetCambiarPar
        ;;
      14)   
        ### His. Mora de 90
        GetCambiarPar
        ;;
      15)   
        ### His. Mora de 120
        GetCambiarPar
        ;;
      16)   
        ### His. Mal Manejo
        GetCambiarPar
        ;;
      17)   
        ### His. Cart. Recuperada
        GetCambiarPar
        ;;
      18)
        ### Mora TDC
        GetCambiarPar
        ;;
      19)
        ### Mora CAB
        GetCambiarPar
        ;;
      20)
        ### Mora Cooperativo
        GetCambiarPar
        ;;
      21)
        ### Mora Hipotecario
        GetCambiarPar
        ;;
      22)
        ### Calificacion Endeudamiento
        GetCambiarPar
        ;;
      23)
        ### Cuantas Embargadas
        GetCambiarPar
        ;;
      24)
        ### Cuentas Canceladas Mal Manejo
        GetCambiarPar
        ;;
      25)
        ### Ingresos
        GetCambiarPar
        ;;
      26)
        ### Capacidad Pago
        GetCambiarPar
        ;;
      27)
        ### Edad
        GetCambiarPar
        ;;
      *)
        ### error
        echo " Opcion  Digitada NO es valida"
        GetEnter
        ;;
    esac
    if [ "$NOPCION" -eq 0 ] ; then

      if [ "$SW_ERROR" -gt 0 ] ; then
        LoopMenu=0
        NOPCION-=1
      fi

    fi
  fi
done
if [ "$NOPCION" -eq 0 ] ; then
  GetArchivo
  SaveParametros
  EjecutaProceso
  exit
fi
#
#*-----------------------------------------------------------------------------*
#            FIN DEL LLAMADO DE FUNCIONES DE EJECUCION MENU PRINCIPAL 
#*-----------------------------------------------------------------------------*
#Eof
