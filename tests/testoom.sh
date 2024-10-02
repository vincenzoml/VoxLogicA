#!/bin/bash 

max="$1"
rm -f int.imgql
echo load img = \"three_coloured_items.png\" >> int.imgql
echo 'let i0 = intensity(img) >. 0' >> int.imgql
echo 'let i1 = intensity(img) >. 100' >> int.imgql


for f in $(seq 2 $max); do echo let i$f = and\(i$(($f - 1)),i$(($f - 2))\) >> int.imgql;done 

echo save \"out.png\" i$max >> int.imgql

../bin/Debug/net5.0/linux-x64/VoxLogicA int.imgql
#../bin/release/net5.0/linux-x64/VoxLogicA int.imgql