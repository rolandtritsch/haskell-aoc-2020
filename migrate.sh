#!/bin/bash

day=${1}
day2=${2}

cp -r ./app/Day01 ./app/Day${day}
cp ./test/Day02Spec.hs ./test/Day${day}Spec.hs 
cp ../hamler-aoc-2020/pdf/Day\ ${day2}\ -\ Advent\ of\ Code\ 2020.pdf pdf
cp ../hamler-aoc-2020/input/Day${day}p1* input
cp ../hamler-aoc-2020/src/Day${day}.hm src/Day${day}.hs 

