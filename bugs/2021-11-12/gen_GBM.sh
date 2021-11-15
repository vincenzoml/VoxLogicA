#!/bin/bash

lorange=82
hirange=92
hiThr=93

cat <<EOF
let dice(x,y) = (2 .*. volume(x & y)) ./. (volume(x) .+. volume(y))
let sensitivity(x,y) = volume(x & y) ./. (volume(x & y) .+. volume((!x) & (y)))
let specificity(x,y) = volume((!x) & (!y)) ./. (volume((!x) & (!y)) .+. volume((x) & (!y)))

let grow(a,b) = (a|touch(b,a))
let smoothen(r,a) = distleq(r,distgeq(r,!a))

let similarTo(r,a,img) = crossCorrelation(r,img,img,a,min(img),max(img),100)

load imgflair = "\$INPUTDIR/\$NAME_flair.nii.gz"
load imgpflair = "\$INPUTDIR/pflair.nii.gz"
load imgbrain = "\$INPUTDIR/brain.nii.gz"
load imgGroundTruthSeg = "\$INPUTDIR/\$NAME_seg.nii.gz"

let flair = intensity(imgflair)
let pflair = intensity(imgpflair)
let brain = intensity(imgbrain) >. 0

let groundTruthSegGTV = intensity(imgGroundTruthSeg) >. 0
let groundTruthSegCTV = distleq(25,groundTruthSegGTV) & brain

let hI = pflair >. 0.$hiThr 
let hyperIntense = smoothen(5.0,hI)
let diceHIgtv = dice(hyperIntense,groundTruthSegGTV)
print "01_dice_hyperIntense_gtv" diceHIgtv
save "\$INPUTDIR/tabulated/hyperIntense@hiThr_0.$hiThr.nii.gz" hyperIntense
EOF

for viThr in $(seq $lorange $hirange); do 

cat <<EOF

// viThr = 0.$viThr

let vI$viThr = pflair >. 0.$viThr
let veryIntense$viThr =  smoothen(2.0,vI$viThr)

let growTum$viThr = grow(hyperIntense,veryIntense$viThr)

let tumSim$viThr = similarTo(5,growTum$viThr,flair)
let tumStatCC$viThr = smoothen(2.0,(tumSim$viThr >. 0.6))

let gtv$viThr = grow(growTum$viThr,tumStatCC$viThr)
let ctv$viThr = distleq(25,gtv$viThr) & brain

let diceVIgtv$viThr = dice(veryIntense$viThr,groundTruthSegGTV)

let diceGrowTum$viThr = dice(growTum$viThr,groundTruthSegGTV)
let sensGrowTum$viThr = sensitivity(growTum$viThr,groundTruthSegGTV)
let specGrowTum$viThr = specificity(growTum$viThr,groundTruthSegGTV)

let diceGTV$viThr = dice(gtv$viThr,groundTruthSegGTV)
let sensGTV$viThr = sensitivity(gtv$viThr,groundTruthSegGTV)
let specGTV$viThr = specificity(gtv$viThr,groundTruthSegGTV)

let diceCTV$viThr = dice(ctv$viThr,groundTruthSegCTV)
let sensCTV$viThr = sensitivity(ctv$viThr,groundTruthSegCTV)
let specCTV$viThr = specificity(ctv$viThr,groundTruthSegCTV)

save "\$INPUTDIR/tabulated/veryIntense@hiThr_0.$hiThr-viThr_0.$viThr.nii.gz" veryIntense$viThr
save "\$INPUTDIR/tabulated/growTum@hiThr_0.$hiThr-viThr_0.$viThr.nii.gz" growTum$viThr

print "02_dice_veryIntense_gtv@0.$viThr" diceVIgtv$viThr
print "03_dice_growTum@0.$viThr" diceGrowTum$viThr
print "04_sens_growTum@0.$viThr" sensGrowTum$viThr
print "05_spec_growTum@0.$viThr" specGrowTum$viThr
// print "06_dice_gtv@0.$viThr" diceGTV$viThr
// print "07_sens_gtv@0.$viThr" sensGTV$viThr
// print "08_spec_gtv@0.$viThr" specGTV$viThr
// print "09_dice_ctv@0.$viThr" diceCTV$viThr
// print "10_sens_ctv@0.$viThr" sensCTV$viThr
// print "11_spec_ctv@0.$viThr" specCTV$viThr

EOF

done