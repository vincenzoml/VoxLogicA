#!/bin/bash

echo "host: " $1
echo "repo: " $2
echo "test: " $3
echo "script: " $4

ssh $1 "cd $2/src && git pull && make && cd tests/$3 && echo && echo && echo TEST: && cat $4.imgql && echo && echo && echo RUNNING: &&  ../../bin/release/net5.0/linux-x64/VoxLogicA $4.imgql" && rsync -av pc-ciancia.isti.cnr.it:$2/src/tests/$3/output .
