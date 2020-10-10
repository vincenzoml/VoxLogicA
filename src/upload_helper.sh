#!/bin/bash

version=$(cat VERSION.txt)
tag="v$version"
files=../releases/VoxLogicA_$version_*.zip
echo $files
assets=$(printf -- "-a %s\n" $files)

hub release create $assets $tag
