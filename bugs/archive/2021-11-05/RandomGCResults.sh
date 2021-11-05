#!/bin/bash

(cd ../src && make build-dbg) || exit 1

for m in cpu gpu; do
fmla=x
rm -rf in-$m.imgql out-$m
cat << EOF >> in-$m.imgql
load img="flair.nii.gz"
let x=intensity(img) >=. 100
EOF

for i in $(seq 1 10); do

cat << EOF >> in-$m.imgql
save "out-$m/$i.nii.gz" $fmla
EOF
fmla="N $fmla"

done

cat << EOF >> in-$m.imgql
//save "out-$m/00_intensity.nii.gz" intensity(img)
//save "out-$m/01_thr.nii.gz" x
//save "out-$m/02_final.nii.gz" $fmla
EOF

done

/home/VoxLogicA/binaries/VoxLogicA_0.6.4.3-experimental_linux-x64/VoxLogicA in-cpu.imgql
../src/bin/Debug/net5.0/linux-x64/VoxLogicA in-gpu.imgql

#diff -qr out-{cpu,gpu}
#echo diff: $?
echo ---
md5sum out-cpu/*
echo ---
md5sum out-gpu/*
echo ---