# Automated contouring of High-Grade Gliomas in skull-stripped MR-FLAIR using VoxLogicA 

This directory contains the necessary files to execute the specification presented in [1,2] using VoxLogicA

The provided `.imgql` script is not directly executable by VoxLogicA, since filenames in `load` and `save` instructions contain variables starting with `$`. This is because the script is actually a template, meant to be launched by a bash script. Just replace the variables manually if you want a valid VoxLogicA session file for small experiments. When you have a valid .imgql file you can run "VoxLogicA file.imgql" and get the results.

However it is easier to run the analysis against a dataset. For that, the additional script "run_analysis.sh" is provided. It should be self-explanatory (run it with no arguments to see the usage instructions). The script expects a dataset of MRI-flair, skull stripped 3D images. The dataset should be in BraTS format (the HGG directory of the BraTS dataset, or subsets of it, are a good starting point), that is, there should be a main directory (say `DatasetX`) with a subdirectory per case, named say `CaseY`, and a file called `CaseY_flair.nii.gz` in each such directory, so that the path of each flair is `DatasetX/CaseY/CaseY_flair.nii.gz`.

Before running the script, you need to download voxlogica (go to www.voxlogica.org). On linux or osx, unpack the .zip file in your home directory, and rename the obtained VoxLogicA_VERSION_linux-x64 to just `VoxLogicA`. Alternatively, unpack it anywhere else and create a symlink pointing to the VoxLogicA executable into your `~/bin`, or `/usr/local/bin` directory. Alternatively, set the VOXLOGICA environment variable to point to the VoxLogicA executable, prior to running the script.

For instance, to run the analysis on the HGG subdirectory of the training dataset from BraTS19, run

```./run-analysis.sh ./MICCAI_BraTS_2019_Data_Training/HGG/ GBM-HGG-TACAS19.imgql```

This will create the `output` directory, where you will find a `.csv` file, which summarises all results obtained via `print` statements in the `.imgql` file, and one subdirectory for each dataset, script and case, containing output images, VoxLogicA log files, and .imgql files with variables replaced for that specific case (so that one can run VoxLogicA directly on it).

Let us know how this works (execution times, crashes, difficulties), and feel free to ask any question!

[1] G. Belmonte, V. Ciancia, D. Latella, M. Massink. VoxLogicA: A Spatial Model Checker for Declarative Image Analysis. TACAS 2019. Lecture Notes in Computer Science, volume 11427. Springer. 2019.

[2] G. Belmonte, D. Latella, M. Massink, M. Biondi, G. De Otto, E. Vanzi, G. Rubino, P. Tini, V. Ciancia.
An explainable algorithm for automatic segmentation of glioblastoma. ESMRMB 2019, 36th Annual Scientific Meeting. Magnetic Resonance Materials in Physics, Biology and Medicine. September 2019, Volume 32, Supplement 1.
