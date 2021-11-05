
rm -rf output* input.imgql

/home/VoxLogicA/scripts/glue_analysis.sh /home/VoxLogicA/datasets/BraTS_2019_HGG_Augmented#2Cases glue.imgql  ./input.imgql output
/home/VoxLogicA/binaries/VoxLogicA_0.6.4.3-experimental_linux-x64/VoxLogicA input.imgql
mv output output-cpu
../src/bin/release/net5.0/linux-x64/VoxLogicA input.imgql
mv output output-gpu

echo ---
md5sum output-cpu/*
echo ---
md5sum output-gpu/*
echo ---