#!/bin/bash

version=$(cat VERSION.txt)
tag="v$version"
echo $version
files=../releases/PolyLogicA_"$version"_*.zip

gh release create $tag $files
