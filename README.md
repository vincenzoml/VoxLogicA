<<<<<<< HEAD
PolyLogicA
==========


This branch contains a fork of VoxLogicA that operates on polyhedra. Write to us for more information, or read the paper below: "Geometric Model Checking of Continuous Space" (authors: Nick Bezhanishvili, Vincenzo Ciancia, David Gabelaia, Gianluca Grilletti, Diego Latella, Mieke Massink) 

https://arxiv.org/abs/2105.06194
=======
# VoxLogicA 2

This is the source code of the new iteration of the spatial model checker VoxLogicA. For now it just does parsing and source-code optimization. 

## Running:

First of all, `cd src`.

To see the help, use `dotnet run --help`. As an example, to see the DAG annotated with the memory labelling, use:

	dotnet run hybrid-BTS.imgql --savelabelling  

The labelling can be saved to a file as follows
>>>>>>> 17c31905644b20952e5086389a070bcf0ce1e558

	dotnet run hybrid-BTS.imgql --savelabelling output/hybrid-BTS.txt
	