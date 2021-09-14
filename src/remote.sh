#!/bin/bash

echo "host: " $1
echo "repo: " $2
echo "test: " $3
echo "script: " $4

ssh $1 "cd $2/src && git pull && make && cd tests/$3 && echo && echo && echo && echo RUNNING: && rm -rf output-{cpu,gpu} && ../../bin/release/net5.0/linux-x64/VoxLogicA $4.imgql && echo && echo TEST: && cat $4.imgql && echo && echo && echo MD5: && md5sum output/* && mv output output-gpu && /home/VoxLogicA/binaries/VoxLogicA_0.6.4.1-experimental_linux-x64/VoxLogicA $4.imgql && mv output output-cpu" && rm -rf output-{cpu,gpu} && rsync -av pc-ciancia.isti.cnr.it:$2/src/tests/$3/output-gpu pc-ciancia.isti.cnr.it:$2/src/tests/$3/output-cpu . && md5sum output-{cpu,gpu}/*
