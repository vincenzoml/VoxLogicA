#!/usr/bin/env bash
# Golden tests for the spatio-temporal flattening.
#
# For every case listed in cases.txt the three passes of the pipeline are run in
# sequence, exactly as temporal.py runs them, and the result of each pass is
# compared with the file recorded in expected/.
#
#   ./run.sh              run every case and report the differences
#   ./run.sh --update     record the current output as the expected one
#   ./run.sh <name> ...   restrict the run to the named cases
#
# A pass that fails is not an error for the harness: its exit code and its
# message are recorded in the golden file just like its output would be, so that
# the tests also pin the behaviour of the bugs that are still open.

set -u

cd "$(dirname "$0")" || exit 1
root=$(cd ../.. && pwd)
binary=$root/src/bin/Release/net8.0/linux-x64/VoxLogicA

update=false
selected=()

for arg in "$@"; do
    case $arg in
    --update) update=true ;;
    -h | --help)
        sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'
        exit 0
        ;;
    -*)
        echo "unknown option: $arg" >&2
        exit 2
        ;;
    *) selected+=("$arg") ;;
    esac
done

if [ ! -x "$binary" ]; then
    echo "building $binary"
    (cd "$root/src" && dotnet build -c Release) >/dev/null || {
        echo "build failed" >&2
        exit 2
    }
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# The log carries a timestamp and the git hash of the build, and a stack trace
# carries absolute paths and line numbers; none of them belong in a golden file.
normalise() {
    sed -e 's/^\[ *[0-9]*ms\]/[TIME]/' \
        -e '/version: .*pre-alpha/d' \
        -e '/^   at /d' \
        -e 's/[[:space:]]*$//'
}

# run <output file> <voxlogica arguments...>
# Prints whatever the run has to say -- a warning is as much a part of the
# behaviour as the translation is -- and then its output, or its exit code.
run() {
    local out=$1
    shift
    local log status diagnostics
    log=$("$binary" "$@" 2>&1)
    status=$?
    diagnostics=$(echo "$log" | normalise | grep -v '^\[TIME\] \[info\]')
    [ -n "$diagnostics" ] && echo "$diagnostics"

    if [ $status -ne 0 ]; then
        echo "EXIT $status"
    elif [ -f "$out" ]; then
        cat "$out"
    else
        echo "EXIT 0 but $(basename "$out") was not written"
    fi
}

generate() {
    local name=$1 frames=$2
    local p1=$work/$name.1.imgql
    local p2=$work/$name.2.imgql
    local p3=$work/$name.3.imgql
    rm -f "$p1" "$p2" "$p3"

    echo "### case $name, $frames frames"
    echo
    echo "### pass 1: --savetaskgraphasprogram --providecontext n"
    run "$p1" cases/"$name".imgql --numframes "$frames" --savetaskgraphasprogram "$p1" --providecontext n
    echo
    echo "### pass 2: --savetaskgraphasprogram"
    if [ -f "$p1" ]; then
        run "$p2" "$p1" --numframes "$frames" --savetaskgraphasprogram "$p2"
    else
        echo "SKIPPED: pass 1 produced no output"
    fi
    echo
    echo "### pass 3: --evaluatespatiotemporal"
    if [ -f "$p2" ]; then
        run "$p3" "$p2" --numframes "$frames" --evaluatespatiotemporal "$p3"
    else
        echo "SKIPPED: pass 2 produced no output"
    fi
}

wanted() {
    [ ${#selected[@]} -eq 0 ] && return 0
    local c
    for c in "${selected[@]}"; do [ "$c" = "$1" ] && return 0; done
    return 1
}

failed=0
total=0

while read -r name frames; do
    case $name in '' | \#*) continue ;; esac
    wanted "$name" || continue
    total=$((total + 1))
    golden=expected/$name.golden
    actual=$work/$name.actual
    generate "$name" "$frames" >"$actual"

    if $update; then
        if [ -f "$golden" ] && cmp -s "$golden" "$actual"; then
            echo "unchanged $name"
        else
            cp "$actual" "$golden"
            echo "recorded  $name"
        fi
    elif [ ! -f "$golden" ]; then
        echo "MISSING   $name (no $golden; run ./run.sh --update)"
        failed=$((failed + 1))
    elif cmp -s "$golden" "$actual"; then
        echo "ok        $name"
    else
        echo "FAILED    $name"
        diff -u "$golden" "$actual" | sed 's/^/    /'
        failed=$((failed + 1))
    fi
done <cases.txt

echo
if $update; then
    echo "$total case(s) recorded"
    exit 0
fi
echo "$((total - failed))/$total case(s) ok"
[ $failed -eq 0 ]
