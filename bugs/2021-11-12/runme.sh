DATASET=/ramdisk/100cases


#PERFORMANCETEST=--performancetest

SRC=../../src
CLASSIC=../../../VoxLogicA.classic

#CONF=Debug
CONF=release

if [ "$CONF" == "Debug" ]; then
    BUILD=build-dbg 
else
    BUILD=build 
fi


OUTPREFIX=/ramdisk/vince
OUTPUT=/ramdisk/output
rm -rf $OUTPUT/* $OUTPREFIX/output-{c,g}pu* $OUTPREFIX/log-*.txt $input.imgql

(cd $SRC && make $BUILD)|| exit 1

#ITER=3

/home/VoxLogicA/scripts/glue_analysis.sh $DATASET ./glue-gpu.imgql  ./input-gpu.imgql $OUTPREFIX/output-gpu  || exit 1
/home/VoxLogicA/scripts/glue_analysis.sh $DATASET ./glue-cpu.imgql  ./input-cpu.imgql $OUTPREFIX/output-gpu  || exit 1


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

($SRC/bin/$CONF/net5.0/linux-x64/VoxLogicA $PERFORMANCETEST input-gpu.imgql | tee $OUTPREFIX/log-gpu.txt) || exit 1
#dot -Tpdf DebugFormulas.dot  > DebugFormulas.pdf

(cd $CLASSIC/src && git checkout tmp2 && git pull && make)|| exit 1
($CLASSIC/src/bin/release/net5.0/linux-x64/VoxLogicA $PERFORMANCETEST input-cpu.imgql | tee $OUTPREFIX/log-cpu.txt) || exit 1

echo --- CPU ---
md5sum $OUTPREFIX/output-cpu/*
echo --- GPU ---
md5sum $OUTPREFIX/output-gpu/*
echo --- END ---

# diff -q $OUTPREFIX/output-cpu $OUTPREFIX/output-gpu
# echo $?

#diff <(grep 'Starting task' out.log|cut -f 2 -d k|cut -b 2-|sort -n) <(grep disposing out.log |cut -f 2 -d g | cut -b 2- |sort -n)|grep '^<'

# diff <(grep user $OUTPREFIX/log-gpu.txt|cut -b 16- |sort) <(grep user $OUTPREFIX/log-cpu.txt | cut -b 16- |sort)
# echo $?