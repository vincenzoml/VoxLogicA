# VoxLogicA 2

This is the source code of the new iteration of the spatial model checker VoxLogicA. For now it just does parsing and source-code optimization. 

## Running:

First of all, `cd src`.

To see the help, use `dotnet run --help`. As an example, to see the DAG annotated with the memory labelling, use:

	dotnet run hybrid-BTS.imgql --savelabelling  

The labelling can be saved to a file as follows

	dotnet run hybrid-BTS.imgql --savelabelling output/hybrid-BTS.txt
	