#!/bin/bash

version=$(cat VERSION.txt)
tag="v$version"
echo $version
files=../releases/VoxLogicA_"$version"_*.zip

gh auth login
gh release create $tag $files
