#!/bin/bash

version=$(cat VERSION.txt)
tag="v$version"
<<<<<<< HEAD
branch=$(git branch --show-current)
echo $version
files=../releases/PolyLogicA_"$version"_*.zip

gh release create $tag --target "$branch" $files
=======
echo $version
files=../releases/VoxLogicA_"$version"_*.zip

gh release create $tag $files
>>>>>>> 17c31905644b20952e5086389a070bcf0ce1e558
