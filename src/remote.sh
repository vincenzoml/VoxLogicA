#!/bin/bash

ssh $1 "cd $2 && git pull && make && cd tests/$3 && ../../bin/release/net5.0/linux-x64/VoxLogicA $4.imgql" && rsync -av pc-ciancia.isti.cnr.it:$2/tests/$3/output .
