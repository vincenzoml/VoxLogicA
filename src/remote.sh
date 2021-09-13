#!/bin/bash

ssh $1 "cd $2 && git pull && make && ./bin/release/net5.0/linux-x64/VoxLogicA test.imgql" && rsync -av pc-ciancia.isti.cnr.it:$2/output .
