#!/bin/bash

version=$(cat VERSION.txt)
tag="v$version"
files=../releases/VoxLogicA_$version_*.zip
echo $files
assets=$(printf -- "-a %s\n" $files)

echo gh release create $tag $assets

echo please modify upload_helper.sh and replace "hub" with "gh release create v0.6.1-experimental VoxLogicA_0.6.1-experimental_*.zip -p --target experimental -t "experimental release with json output (--json command line option)" -n "please use the most recent stable release 0.6.0 instead"
