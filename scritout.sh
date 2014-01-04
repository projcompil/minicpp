#!/bin/bash

path=/home/nm/Documents/tests
pathbin=/home/nm/Documents/minicpp/minic++
pathstore=/tmp/
nomferreurs=erreurs_recensees
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

function compi {
	name=$(basename "$1")
	name="${name%.*}"
	$pathbin $i -o "$pathstore$name.s"
	if [ $? == 0 ] ; then
		spim -f "$pathstore$name.s" | tail -n +6 > "$pathstore$name.out"
		d="$(diff "$pathstore$name.out" "$name.out")"
		if [ -z "$d" ] ; then
			echo -e "Réussite de la compilation du fichier $1\\n"
			return 0
		else
			echo -e "Echec de la compilation du fichier $1\\n"
			echo $d
			return 1
		fi
	else
		return 3
	fi
}

function app {
	optionv=$3
	cd $path/$1
	echo -e "$1\\n"
	for i in *.cpp ; do
		echo "Fichier : $i"
		if [ -z "$argu" ] ; then
			compi $i
		else	
			$pathbin $2 $i #> /dev/null
		fi
		retour=$?
		echo -e "$retour\\n"
		if [ $retour == $[$optionv-1] ] ; then
			comptc=$[comptc+1]
		elif [ $retour == 3 ] ; then
			comptni=$[comptni+1]
		
		else
			echo "$path$1/$i" >> "$pathstore$nomferreurs"
		fi
		compt=$[compt+1]
	done
	echo -e "\\n\\n"
}

echo -e "option choisie : $argu \\n"

if [ "$argu" == "--parse-only" ] ; then
	app "/syntax/good" "--parse-only" 1

	app "/syntax/bad" $argu 2

	app "/typing/good" $argu 1

	app "/typing/bad" $argu 1

elif [ "$argu" == "--type-only" ] ; then
	app "/typing/good" $argu 1

        app "/typing/bad" $argu 2

fi

app "/exec" "$argu" 1

echo -e "\\n\\n(réussites = $comptc, échecs = $[compt-comptc] dont non implémenté : $comptni)\\n"

echo -e "Les fichiers provoquant des erreurs sont :\\n"
cat "$pathstore$nomferreurs"
rm "$pathstore$nomferreurs"
