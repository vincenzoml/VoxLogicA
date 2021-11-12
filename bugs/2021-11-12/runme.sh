SRC=../../src

# CONF=Debug      
# BUILD=build-dbg 
CONF=release      
BUILD=build 

DATASET=/ramdisk/259cases

OUTPREFIX=/ramdisk/vince
OUTPUT=/ramdisk/output
rm -rf $OUTPUT/* $OUTPREFIX/output-cpu* $OUTPREFIX/output-gpu* input.imgql

(cd $SRC && make $BUILD)|| exit 1

#ITER=3

#git checkout gpu-new

/home/VoxLogicA/scripts/glue_analysis.sh $DATASET ./glue.imgql  ./input.imgql $OUTPUT  || exit 1

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

$SRC/bin/$CONF/net5.0/linux-x64/VoxLogicA input.imgql | tee out.log && mv $OUTPUT $OUTPREFIX/output-gpu
#dot -Tpdf DebugFormulas.dot  > DebugFormulas.pdf

#git checkout tmp2 || exit 1
#(cd $SRC && make)|| exit 1
#$SRC/bin/release/net5.0/linux-x64/VoxLogicA input.imgql || (git checkout gpu-new; exit 1)
#mv $OUTPUT $OUTPREFIX/output-cpu
#
#git checkout gpu-new
#
#echo --- CPU ---
#md5sum $OUTPREFIX/output-cpu/*
#echo --- GPU ---
#md5sum $OUTPREFIX/output-gpu/*
#echo --- END ---

# diff -q $OUTPREFIX/output-cpu $OUTPREFIX/output-gpu
# echo $?

#diff <(grep 'Starting task' out.log|cut -f 2 -d k|cut -b 2-|sort -n) <(grep disposing out.log |cut -f 2 -d g | cut -b 2- |sort -n)|grep '^<'