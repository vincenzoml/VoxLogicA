DATASET=/ramdisk/259cases




SRC=../../src
CLASSIC=../../../VoxLogicA.classic

#CONF=Debug      
#BUILD=build-dbg 
CONF=release      
BUILD=build 


OUTPREFIX=/ramdisk/vince
OUTPUT=/ramdisk/output
rm -rf $OUTPUT/* $OUTPREFIX/output-cpu* $OUTPREFIX/output-gpu* input.imgql

(cd $SRC && make $BUILD)|| exit 1

#ITER=3

#git checkout gpu-new

/home/VoxLogicA/scripts/glue_analysis.sh $DATASET ./glue-gpu.imgql  ./input-gpu.imgql $OUTPUT  || exit 1
/home/VoxLogicA/scripts/glue_analysis.sh $DATASET ./glue-cpu.imgql  ./input-cpu.imgql $OUTPUT  || exit 1


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

$SRC/bin/$CONF/net5.0/linux-x64/VoxLogicA input-gpu.imgql | tee out.log && mv $OUTPUT $OUTPREFIX/output-gpu
#dot -Tpdf DebugFormulas.dot  > DebugFormulas.pdf

# (cd $CLASSIC/src && make)|| exit 1
# $CLASSIC/src/bin/release/net5.0/linux-x64/VoxLogicA input-cpu.imgql || exit 1
# mv $OUTPUT $OUTPREFIX/output-cpu

# echo --- CPU ---
# md5sum $OUTPREFIX/output-cpu/*
# echo --- GPU ---
# md5sum $OUTPREFIX/output-gpu/*
# echo --- END ---

# diff -q $OUTPREFIX/output-cpu $OUTPREFIX/output-gpu
# echo $?

#diff <(grep 'Starting task' out.log|cut -f 2 -d k|cut -b 2-|sort -n) <(grep disposing out.log |cut -f 2 -d g | cut -b 2- |sort -n)|grep '^<'