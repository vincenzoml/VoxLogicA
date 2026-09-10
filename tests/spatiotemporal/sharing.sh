#!/usr/bin/env bash
# Measures what the flattening costs per frame.
#
#   ./sharing.sh [specification.imgql] [frame counts...]
#
# The flattening turns a spatio-temporal specification into a purely spatial one
# over N frames. This reports how many operations that comes to, against the same
# expansion with the memoisation of the reducer switched off, which is what the
# specification costs when no subterm is shared with any other.
#
# The second figure needs a build of the tool that does not memoise. Rather than
# edit the sources in place, a copy of them is built in a temporary directory, so
# running this leaves the working tree alone.

set -u

cd "$(dirname "$0")" || exit 1
root=$(cd ../.. && pwd)
binary=$root/src/bin/Release/net8.0/linux-x64/VoxLogicA

# The numbers are the point of this script, so they are formatted the same way
# wherever it runs, rather than with the decimal separator of the machine.
export LC_ALL=C

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

spec=${1:-}
[ $# -gt 0 ] && shift
frames=("$@")
[ ${#frames[@]} -eq 0 ] && frames=(1 2 5 10 20 50)

# A specification of the shape a lesion tracking one would have: an intensity
# shared by two thresholds, a region built from both, used twice, under an until.
if [ -z "$spec" ]; then
    spec=$work/lesion.imgql
    cat >"$spec" <<'EOF'
let study = load("study.nii.gz")
let i = intensity(frame(study,n))
let lesion = gt(i,500)
let edema = gt(i,300)
let core = and(lesion,not(mayReach(edema,lesion)))
let critical = touch(core,edema)
save "r" until(core,critical)
EOF
fi

if [ ! -x "$binary" ]; then
    echo "building $binary"
    (cd "$root/src" && dotnet build -c Release) >/dev/null || {
        echo "build failed" >&2
        exit 2
    }
fi

echo "building a copy of the tool without memoisation"
mkdir -p "$work/src"
cp "$root/src"/*.fs "$root/src"/*.fsproj "$root/src"/VERSION.txt "$work/src/"
sed -i 's/^    let memoize = true$/    let memoize = false/' "$work/src/Reducer.fs"
grep -q "let memoize = false" "$work/src/Reducer.fs" || {
    echo "could not find the memoisation switch in Reducer.fs" >&2
    exit 2
}
(cd "$work/src" && dotnet build -c Release) >/dev/null || {
    echo "the copy did not build" >&2
    exit 2
}
unshared=$work/src/bin/Release/net8.0/linux-x64/VoxLogicA

# The size of the flattened specification: the operations the second pass reduces
# the output of the first one to, which is what VoxLogicA would go on to compute.
flattened() {
    local tool=$1 n=$2
    "$tool" "$spec" --numframes "$n" --providecontext n --savetaskgraphasprogram "$work/pass1.imgql" >/dev/null 2>&1 || return 1
    "$tool" "$work/pass1.imgql" --numframes "$n" --savetaskgraphasprogram "$work/pass2.imgql" 2>&1 >/dev/null |
        sed -n 's/.*Number of tasks: \([0-9]*\).*/\1/p'
}

source_size=$("$binary" "$spec" --numframes 1 --providecontext n --savetaskgraphasprogram /dev/null 2>&1 >/dev/null |
    sed -n 's/.*Number of tasks: \([0-9]*\).*/\1/p')

echo
echo "specification: $(basename "$spec"), $source_size operations before flattening"
echo
printf '%8s  %10s  %12s  %7s\n' frames flattened "no sharing" ratio
printf '%8s  %10s  %12s  %7s\n' ------ --------- ------------ -----

for n in "${frames[@]}"; do
    a=$(flattened "$binary" "$n")
    b=$(flattened "$unshared" "$n")
    if [ -z "$a" ] || [ -z "$b" ]; then
        printf '%8s  %10s  %12s  %7s\n' "$n" "${a:-failed}" "${b:-failed}" -
        continue
    fi
    printf '%8s  %10s  %12s  %7s\n' "$n" "$a" "$b" "$(awk -v x="$b" -v y="$a" 'BEGIN{printf "%.1fx", x/y}')"
    first_n=${first_n:-$n} first_a=${first_a:-$a} first_b=${first_b:-$b}
    last_n=$n last_a=$a last_b=$b
done

# Both grow linearly with the number of frames; what the sharing changes is the
# slope, which is the interesting figure and the one that belongs in a paper.
if [ "${last_n:-0}" != "${first_n:-0}" ]; then
    echo
    awk -v n0="$first_n" -v n1="$last_n" -v a0="$first_a" -v a1="$last_a" -v b0="$first_b" -v b1="$last_b" 'BEGIN {
        sa = (a1 - a0) / (n1 - n0)
        sb = (b1 - b0) / (n1 - n0)
        printf "operations per frame: %.1f flattened, %.1f without sharing (%.1fx)\n", sa, sb, sb / sa
        print  "both are linear in the number of frames: the sharing is a constant factor,"
        print  "and it is the number of distinct spatial subterms of the specification."
    }'
fi
