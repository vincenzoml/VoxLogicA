#!/bin/bash
(cd .. && make)
../bin/release/net5.0/linux-x64/VoxLogicA input.imgql
mv output output-gpu
/home/VoxLogicA/binaries/VoxLogicA_0.6.4.1-experimental_linux-x64/VoxLogicA input.imgql
mv output output-cpu

md5sum output*/*
