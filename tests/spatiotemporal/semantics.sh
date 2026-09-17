#!/usr/bin/env bash
# The check the goldens cannot make: runs the programs the pipeline emits, with
# VoxLogicA 1, on the frames drawn in frames/, and compares what they save with
# the images drawn there as their expected results.
#
#   ./semantics.sh              run every case that has an expected image
#   ./semantics.sh merge-tracked
#
# A case is run the way a driver would run it. A quantified case is first run
# as a probe, whose prints give the highest label of each labelling; their
# maximum is the bound the specification is then unrolled with, so that the
# bound is measured on the data rather than read from cases.txt. The program of
# the third pass is run next to a copy of the frames and of stdlib2.imgql, and
# the check of the bound it prints has to come out true. The comparison of the
# saved image with the expected one is itself a VoxLogicA 1 program, after
# thresholding: the two images have to agree voxel by voxel.
#
# VoxLogicA 1 is fetched by vl1.sh if it is not there yet.

set -u

cd "$(dirname "$0")" || exit 1
root=$(cd ../.. && pwd)
binary=$root/src/bin/Release/net8.0/linux-x64/VoxLogicA

if [ ! -x "$binary" ]; then
    (cd "$root/src" && dotnet build -c Release) >/dev/null || {
        echo "build failed" >&2
        exit 2
    }
fi

vl1=$(./vl1.sh) || exit 2

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# The frames are drawn afresh, so that the images in frames/ and what the
# cases run on cannot drift apart.
(cd frames && ./draw.py) || exit 2
mkdir -p "$work/frames"
cp frames/*.png "$work/frames/"
cp "$root/src/stdlib2.imgql" "$work/"

# vl1 <program>: runs VoxLogicA 1 in the work directory and prints its "print"
# lines as name=value, one per line, dropping the log around them.
vl1() {
    (cd "$work" && "$vl1" "$1" 2>&1) | sed -n 's/^\[ *[0-9]*ms\] \[user\] //p'
}

# flatten <case> <frames> <output> [<maxlabels args...>]: the three passes.
flatten() {
    local name=$1 frames=$2 out=$3
    shift 3
    "$binary" cases/"$name".imgql --numframes "$frames" --providecontext n "$@" --savetaskgraphasprogram "$work/$name.1.imgql" >/dev/null 2>&1 &&
        "$binary" "$work/$name.1.imgql" --numframes "$frames" --savetaskgraphasprogram "$work/$name.2.imgql" >/dev/null 2>&1 &&
        "$binary" "$work/$name.2.imgql" --numframes "$frames" --evaluatespatiotemporal "$out" >/dev/null 2>&1
}

failed=0
total=0

for expected in frames/*.expected.png; do
    name=$(basename "$expected" .expected.png)
    if [ $# -gt 0 ]; then
        wanted=false
        for c in "$@"; do [ "$c" = "$name" ] && wanted=true; done
        $wanted || continue
    fi
    total=$((total + 1))
    k=

    line=$(grep -E "^$name[[:space:]]" cases.txt)
    frames=$(echo "$line" | awk '{print $2}')
    quantified=$(echo "$line" | awk '{print $3}')
    if [ -z "$frames" ]; then
        echo "FAILED    $name: not in cases.txt"
        failed=$((failed + 1))
        continue
    fi

    # The bound: measured by the probe when the case quantifies over labels.
    bound=()
    if [ -n "$quantified" ]; then
        if ! flatten "$name" "$frames" "$work/$name.probe.imgql" --probe; then
            echo "FAILED    $name: the probe did not flatten"
            failed=$((failed + 1))
            continue
        fi
        # VoxLogicA 1 prints a number as 2.0, and the bound is an integer.
        k=$(vl1 "$name.probe.imgql" | sed -n 's/^labels of .*=//p' | sort -n | tail -1 | sed 's/\.0*$//')
        if [ -z "$k" ]; then
            echo "FAILED    $name: the probe printed no label count"
            failed=$((failed + 1))
            continue
        fi
        bound=(--maxlabels "$k")
    fi

    if ! flatten "$name" "$frames" "$work/$name.imgql" "${bound[@]}"; then
        echo "FAILED    $name: did not flatten"
        failed=$((failed + 1))
        continue
    fi

    prints=$(vl1 "$name.imgql")
    if [ ! -f "$work/$name.png" ]; then
        echo "FAILED    $name: VoxLogicA 1 saved nothing"
        (cd "$work" && "$vl1" "$name.imgql" 2>&1 | grep -v '\[info\]' | sed 's/^/    /')
        failed=$((failed + 1))
        continue
    fi

    # Every check of a bound has to come out true.
    if echo "$prints" | grep -q '^labels op[0-9]* within [0-9]*=false'; then
        echo "FAILED    $name: a bound was exceeded"
        echo "$prints" | sed 's/^/    /'
        failed=$((failed + 1))
        continue
    fi

    # The comparison, by the model checker: the saved image and the expected
    # one, thresholded, have to hold at exactly the same voxels.
    cat >"$work/$name.compare.imgql" <<END
import "stdlib2.imgql"
load got = "$name.png"
load want = "frames/$name.expected.png"
let a = gt(intensity(got),127)
let b = gt(intensity(want),127)
print "same" eqB(a,b)
print "unexpected" volume(and(a,not(b)))
print "missing" volume(and(b,not(a)))
END
    verdict=$(vl1 "$name.compare.imgql")
    if echo "$verdict" | grep -q '^same=true'; then
        echo "ok        $name${k:+ (bound measured: $k)}"
    else
        echo "FAILED    $name: the result differs from frames/$name.expected.png"
        echo "$verdict" | grep -v '^same=' | sed 's/^/    voxels /'
        failed=$((failed + 1))
    fi
done

echo
echo "$((total - failed))/$total case(s) ok"
[ $failed -eq 0 ]
