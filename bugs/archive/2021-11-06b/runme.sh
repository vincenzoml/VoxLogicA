SRC=../../../src

(cd $SRC && make)|| exit 1

#ITER=3

git checkout gpu-new
rm -rf output* input.imgql

/home/VoxLogicA/scripts/glue_analysis.sh ./BraTS_2019_HGG_Augmented#40cases ./glue.imgql  ./input.imgql output || exit 1

# for i in $(seq 1 $ITER); do
#     while ! (timeout -s KILL 30 ../../src/bin/release/net5.0/linux-x64/VoxLogicA input.imgql && mv output output-gpu-$i) ; do
        
#         rm -rf output
#         if test -f exit; then
#             exit 132
#         fi
#     done



# done


# for i in $(seq 2 $ITER); do
#     echo comparing iteration 1 to $i
#     diff -qr output-gpu-1 output-gpu-$i
#     echo $?
# done

($SRC/bin/release/net5.0/linux-x64/VoxLogicA input.imgql && mv output output-gpu) || exit 1

# dot -Tpdf DebugFormulas.dot  > DebugFormulas.pdf

git checkout experimental || exit 1
(cd $SRC && make)|| exit 1
$SRC/bin/release/net5.0/linux-x64/VoxLogicA input.imgql || (git checkout gpu-new; exit 1)
mv output output-cpu

git checkout gpu-new

echo --- CPU ---
md5sum output-cpu/*
echo --- GPU ---
md5sum output-gpu/*
echo --- END ---

diff -q output-cpu output-gpu
echo $?