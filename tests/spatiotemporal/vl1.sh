#!/usr/bin/env bash
# Fetches a stable release of VoxLogicA 1, the model checker the pipeline emits
# programs for, into vl1/ next to this script, and prints the path of the
# binary. The release is self-contained (SimpleITK included), so nothing else
# has to be installed; vl1/ is ignored by git.
#
#   ./vl1.sh            fetch if not there yet, print the path of the binary
#   ./vl1.sh --clean    remove the download
#
# The version is pinned: the generated programs are checked against the
# expected images of frames/, and a change of model checker is something to see.

set -eu

cd "$(dirname "$0")" || exit 1

version=1.3.3-experimental
archive=VoxLogicA_${version}_linux-x64.zip
url=https://github.com/vincenzoml/VoxLogicA/releases/download/v${version}/${archive}
dir=vl1
binary=$dir/VoxLogicA_${version}_linux-x64/VoxLogicA

if [ "${1:-}" = "--clean" ]; then
    rm -rf "$dir"
    exit 0
fi

if [ ! -x "$binary" ]; then
    mkdir -p "$dir"
    echo "fetching VoxLogicA $version into $dir/" >&2
    curl -L --fail --progress-bar -o "$dir/$archive" "$url"
    unzip -q -o "$dir/$archive" -d "$dir"
    rm -f "$dir/$archive"
    chmod +x "$binary"
    [ -x "$binary" ] || {
        echo "the archive did not contain $binary" >&2
        exit 2
    }
fi

echo "$(pwd)/$binary"
