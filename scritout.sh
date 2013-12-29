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
		 $pathbin $2 $i
	done
	echo -e "\\n\\n"
}

echo -e "option choisie : $argu \\n"


app "/syntax/good" "--parse-only"

app "/syntax/bad" $argu

app "/typing/good" $argu

app "/typing/bad" $argu

app "/exec" $argu

# $pathbin $i "/tmp/$i.s"
# spim "/tmp/$i.s" | tail -n +6 > "/tmp/$i.out"
# d = diff "/tmp/$i.out" $ioutduprof
# if [ -z $d ] ; then
#   echo -e "Réussite de la compilation du fichier $i\\n"
# else
#    echo -e "Echec de la compilation du fichier $i\\n"
#    echo $d
# fi
