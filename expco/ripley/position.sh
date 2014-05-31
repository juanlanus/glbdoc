#   clear
#   echo -en "\033[s\033[7B\033[1;34m BASH BASH\033[u\033[0m"

    echo " "
    echo " "

    endLoop="0"
    while [ $endLoop == "0" ]
    do
    echo "      1) Origen de la base: I:interna, C:cliente", enter para cancelar
    echo "                 BASE: \c" 
    read BASEnew
    if [ $BASEnew == "c" ]
    then
        BASEnew="C"
    fi
    if [ $BASEnew == "i" ]
    then
        BASEnew="I"
    fi

    if [ $BASEnew == "C" ] || [ $BASEnew == "I" ]
    then
        BASE=$BASEnew
        echo "BASE="$BASE
        endLoop="1"
    else
        if [ -z $BASEnew ]
        then
        endLoop="1"
        exit
        else
        echo -en "\033[5a"
        echo -en "\033[s\033[7B\033[1;34m BASH BASH\033[u\033[0m"
        fi
    fi
    done
exit

$ position.sh


      1) Origen de la base: I:interna, C:cliente, enter para cancelar
                 BASE: \c
w
      1) Origen de la base: I:interna, C:cliente, enter para cancelar
                 BASE: \c

./position.sh: line 13: [: ==: unary operator expected
./position.sh: line 17: [: ==: unary operator expected
./position.sh: line 22: [: ==: unary operator expected
./position.sh: line 22: [: ==: unary operator expected

               \a      Alert character.

               \b      Backspace.

               \c      Print line without new-line.  All  charac-
                       ters  following the \c in the argument are
                       ignored.

               \f      Form-feed.

               \n      New-line.

               \r      Carriage return.

               \t      Tab.

               \v      Vertical tab.

               \\      Backslash.

               \0n     Where n is the 8-bit character whose ASCII
                       code is the 1-, 2- or 3-digit octal number
                       representing that character.
