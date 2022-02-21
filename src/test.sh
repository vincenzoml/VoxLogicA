#!/bin/bash

make && ./bin/release/net5.0/linux-x64/PolyLogicA test.imgql && diff result.json result-correct.json && echo test OK