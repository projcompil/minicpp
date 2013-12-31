#!/bin/bash

path=/home/nm/Documents/tests
pathbin=/home/nm/Documents/minicpp/minic++


if [ "$1" ==  "p" ]; then
	argu="--parse-only"
elif [ "$1" == "t" ]; then
	argu="--type-only"
else
	argu=""
fi

function app {
	cd $path/$1
	echo -e "$1\\n"
	for i in *.cpp ; do
		echo "Fichier : $i"
		$pathbin $2 $i #> /dev/null
		echo -e "$?\\n"
	done
	echo -e "\\n\\n"
}

echo -e "option choisie : $argu \\n"


#app "/syntax/good" "--parse-only"

#app "/syntax/bad" $argu

app "/typing/good" $argu

app "/typing/bad" $argu

app "/exec" $argu

function compi {
	name="{$1%.*}"
	$pathbin $i "/tmp/$name.s"
	spim "/tmp/$name.s" | tail -n +6 > "/tmp/$name.out"
	d = diff "/tmp/$name.out" "$name.out" 
	if [ -z "$d" ] ; then
		echo -e "Réussite de la compilation du fichier $1\\n"
	else
		echo -e "Echec de la compilation du fichier $1\\n"
		echo $d
	fi
}
