(cd /home/vincenzo/data/local/repos/VoxLogicA/src && make) || exit 1 

export VOXLOGICA_PRINT_LOGS=1
/home/VoxLogicA/scripts/gen_GBM_multi-test-GPU.sh > /home/VoxLogicA/specifications/gen_GBM-test-GPU.imgql
time VOXLOGICA=/home/vincenzo/data/local/repos/VoxLogicA/src/bin/release/net5.0/linux-x64/VoxLogicA /home/VoxLogicA/scripts/run_analysis.sh BraTS_2019_HGG_Augmented#1case /home/VoxLogicA/specifications/gen_GBM-test-GPU.imgql output-gpu
#time /home/VoxLogicA/scripts/run_analysis.sh BraTS_2019_HGG_Augmented#1case /home/VoxLogicA/specifications/gen_GBM-test-GPU.imgql output-gpu
