#!/usr/bin/env bash
# Checks on the command line itself, as opposed to the translation: the golden
# tests in run.sh always invoke the tool with the same argument order, so they
# cannot see a command that reads argv by position instead of asking the parser.

set -u

cd "$(dirname "$0")" || exit 1
root=$(cd ../.. && pwd)
binary=$root/src/bin/Release/net8.0/linux-x64/VoxLogicA
spec=cases/until-final.imgql

if [ ! -x "$binary" ]; then
    (cd "$root/src" && dotnet build -c Release) >/dev/null || {
        echo "build failed" >&2
        exit 2
    }
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

failed=0

check() {
    local what=$1 expected=$2 got=$3
    if [ "$expected" = "$got" ]; then
        echo "ok        $what"
    else
        echo "FAILED    $what"
        echo "    expected: $expected"
        echo "    got:      $got"
        failed=$((failed + 1))
    fi
}

# Every command that unrolls the temporal operators needs the number of frames,
# and has to take it from --numframes wherever it appears on the command line.
"$binary" "$spec" --savetaskgraphasast "$work/a.ast" --numframes 3 >/dev/null 2>&1
check "--savetaskgraphasast, flag last" 0 $?

"$binary" --numframes 3 --savetaskgraphasast "$work/b.ast" "$spec" >/dev/null 2>&1
check "--savetaskgraphasast, flag first" 0 $?

if cmp -s "$work/a.ast" "$work/b.ast"; then
    echo "ok        the two argument orders agree"
else
    echo "FAILED    the two argument orders produce different output"
    failed=$((failed + 1))
fi

# A missing --numframes has to be reported as such, not crash on a number that
# was never a number.
message=$("$binary" "$spec" --savetaskgraphasast "$work/c.ast" 2>&1 | grep -c "missing argument '--numframes'")
check "missing --numframes is reported" 1 "$message"

# The commands that do not unroll anything must keep working without it.
"$binary" "$spec" --savetaskgraphasdot "$work/d.dot" >/dev/null 2>&1
check "--savetaskgraphasdot without --numframes" 0 $?

# A horizon of zero frames is meaningless, and has to be reported before any
# command tries to unroll a specification over it.
message=$("$binary" "$spec" --numframes 0 --savetaskgraphasprogram "$work/f.imgql" 2>&1 | grep -c 'fewer than one frame')
check "--numframes 0 is rejected" 1 "$message"

# The bound on the labels is a hypothesis of the run, not a fact about the data:
# a specification that quantifies over labels has to be given one, and one that
# does not must not be asked for it. The case above has no exists in it.
quantified=cases/exists-footprint.imgql
message=$("$binary" "$quantified" --numframes 2 --providecontext n --savetaskgraphasprogram "$work/h.imgql" 2>&1 | grep -c 'has to be given with --maxlabels')
check "missing --maxlabels is reported when exists is used" 1 "$message"

message=$("$binary" "$quantified" --numframes 2 --providecontext n --maxlabels 0 --savetaskgraphasprogram "$work/i.imgql" 2>&1 | grep -c 'fewer than one label')
check "--maxlabels 0 is rejected" 1 "$message"

"$binary" "$spec" --numframes 2 --providecontext n --savetaskgraphasprogram "$work/j.imgql" >/dev/null 2>&1
check "--savetaskgraphasprogram without --maxlabels, when nothing is quantified" 0 $?

# The frame reference is honoured by both dumps of the task graph.
"$binary" "$spec" --numframes 2 --providecontext n --savetaskgraphasast "$work/e.ast" >/dev/null 2>&1
found=$(grep -c 'Declaration ("op0", \["n"\]' "$work/e.ast" 2>/dev/null || echo 0)
check "--providecontext reaches --savetaskgraphasast" 1 "$found"

# Given no file name, a command writes its result to standard output. This used
# to go to the debug log, which a release build compiles away: the command
# printed nothing and exited successfully. Counting lines is not enough to see
# that, since the log itself used to land there too, so count declarations.
"$binary" "$spec" --numframes 2 --providecontext n --savetaskgraphasprogram >"$work/stdout.imgql" 2>/dev/null
declarations=$(grep -c '^let op' "$work/stdout.imgql")
[ "$declarations" -gt 1 ] && declarations=many
check "a command with no file name writes to standard output" many "$declarations"

# And the log is not there to spoil the redirection: it goes to standard error.
check "the log stays out of standard output" 0 "$(grep -c '^\[' "$work/stdout.imgql")"

# Which together mean the redirected output is a specification, and parses back.
"$binary" "$work/stdout.imgql" --savetaskgraphasdot "$work/g.dot" >/dev/null 2>&1
check "what it writes there parses back" 0 $?

echo
[ $failed -eq 0 ] && echo "all cli checks ok" || echo "$failed cli check(s) failed"
[ $failed -eq 0 ]
