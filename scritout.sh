#!/bin/bash

path=/home/nm/Documents/tests
pathbin=/home/nm/Documents/minicpp/minic++

comptc=0
comptni=0
compt=0
retour=0


if [ "$1" ==  "p" ]; then
	argu="--parse-only"
elif [ "$1" == "t" ]; then
	argu="--type-only"
else
	argu=""
fi

function app {
	optionv=$3
	cd $path/$1
	echo -e "$1\\n"
	for i in *.cpp ; do
		echo "Fichier : $i"
		if [ -z "$2" ] ; then
			compi $i
		else	
			$pathbin $2 $i #> /dev/null
			retour=$?
		fi
		echo -e "$retour\\n"
		if [ $retour == $[optionv-1] ] ; then
			comptc=$[comptc+1]
		elif [ $retour == 3 ] ; then
			comptni=$[comptni+1]
		fi
		compt=$[compt+1]
	done
	echo -e "\\n\\n"
}

echo -e "option choisie : $argu \\n"


#app "/syntax/good" "--parse-only"

#app "/syntax/bad" $argu 2

app "/typing/good" $argu 1

app "/typing/bad" $argu 2

app "/exec" $argu 1

echo -e "\\n\\n(réussites = $comptc, échecs = $[compt-comptc] dont non implémenté : $comptni)\\n"

function compi {
	name="{$1%.*}"
	$pathbin $i -o "/tmp/$name.s"
	retour=$?
	spim "/tmp/$name.s" | tail -n +6 > "/tmp/$name.out"
	d = diff "/tmp/$name.out" "$name.out" 
	if [ -z "$d" ] ; then
		echo -e "Réussite de la compilation du fichier $1\\n"
	else
		echo -e "Echec de la compilation du fichier $1\\n"
		echo $d
	fi
}
