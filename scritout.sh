#!/bin/bash

path=/home/nm/Documents/tests
pathbin=/home/nm/Documents/minicpp/minic++
pathstore=/tmp/
nomferreurs=erreurs_recensees
nomfnotimplem=notimplemented
nomfreussites=reussites
comptc=0
comptni=0
compt=0
retour=0
outil0="spim -f"
outil1="java -jar /home/nm/Documents/Mars4_4.jar"

if [ "$1" ==  "p" ]; then
	argu="--parse-only"
elif [ "$1" == "t" ]; then
	argu="--type-only"
else
	argu=""
fi

if [ -z "$1"] ; then
	outilc=0
else
	outilc=1
fi

echo "" > "$pathstore$nomferreurs"
echo "" > "$pathstore$nomfnotimplem"
echo "" > "$pathstore$nomfreussites"

function compi {
	name=$(basename "$1")
	name="${name%.*}"
	$pathbin $i -o "$pathstore$name.asm"
	if [ $? == 0 ] ; then
		if [ $2 == 0 ] ; then
			$outil0 "$pathstore$name.asm" | tail -n +6 > "$pathstore$name.out"
		else
			$outil1 "$pathstore$name.asm" | tail -n +3 | head -n -1 > "$pathstore$name.out"	
		fi
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
			compi $i $outilc
		else	
			$pathbin $2 $i #> /dev/null
		fi
		retour=$?
		echo -e "$retour\\n"
		if [ $retour == $[$optionv-1] ] ; then
			comptc=$[comptc+1]
			echo  "$path$1/$i" >> "$pathstore$nomfreussites" 
		elif [ $retour == 3 ] ; then
			comptni=$[comptni+1]
			echo "$path$1/$i" >> "$pathstore$nomfnotimplem"
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


echo "Les fichiers contenant des caractéristiques non implémentées sont : "
cat "$pathstore$nomfnotimplem"



echo -e "\\n\\n(réussites = $comptc, échecs = $[compt-comptc] dont non implémenté : $comptni)\\n"

echo -e "\\n\\nLes fichiers provoquant des erreurs sont :"
cat "$pathstore$nomferreurs"
#rm "$pathstore$nomferreurs"
