#!/bin/bash

version=$(cat VERSION.txt)
tag="v$version"
echo $version
files=../releases/VoxLogicA_"$version"_*.zip
echo $files
assets=$(printf -- "-a %s\n" $files)

gh release create -d $tag $assets
