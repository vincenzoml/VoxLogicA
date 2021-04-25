#!/bin/bash

version=$(cat VERSION.txt)
tag="v$version"
branch=$(git branch --show-current)
echo $version
files=../releases/PolyLogicA_"$version"_*.zip

gh release create $tag --target "$branch" $files
