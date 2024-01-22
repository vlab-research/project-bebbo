#!/bin/sh

rm -f png/*

for file in *.tex; do
  newfile="${file// /-}"
  newfile="${newfile,,}"
  newfile="${newfile//\.tex/}"
  cat "${file}" | tex2png -o "png/${newfile}.png" -D 300 -T tight
done
