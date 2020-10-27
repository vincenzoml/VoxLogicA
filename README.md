VoxLogicA: Voxel-based Logical Analyser
=======================================

VoxLogicA is a tool for analysing images using ImgQL specifications.

This file documents the main variant of VoxLogicA, which is tested and actively maintained. Other variants exist:

	- if you are looking for the -GPU variant, check out the branch "experimental-gpu" branch
	- if you are looking for the variant that operates on graphs, not just images, check out the "graphs" branch

ImgQL is a language to analyse images, based on spatial logics. VoxLogicA
interprets ImgQL specifications using a model-checking approach, which entails,
in particular, heavy use of memoisation techniques to permit complex analyses in
short execution times, at the expense of used memory. Analysis is automatically
distributed to multiple CPU cores where possible, so that a VoxLogicA session is
typically faster than an equivalent hand-written computer program.

See also: http://www.voxlogica.org 

VoxLogicA is distributed under a permissive open source license. See the file
LICENSE.txt for details.

This file is meant to be an user manual for the tool. However, one can also
refer to the following research paper, which illustrates the philosophy of the
tool in more detail, with a detailed case study.

https://link.springer.com/chapter/10.1007/978-3-030-17462-0_16

If you use VoxLogicA in your research, please consider citing the relevant paper(s):

Belmonte G., Ciancia V., Latella D., Massink M. (2019) VoxLogicA: A Spatial Model Checker for Declarative Image Analysis. 
In: Vojnar T., Zhang L. (eds) Tools and Algorithms for the Construction and Analysis of Systems. 
TACAS 2019. Lecture Notes in Computer Science, vol 11427. Springer, Cham

```
@InProceedings{BCLM2019TACAS,
	author="Belmonte, Gina and Ciancia, Vincenzo and Latella, Diego and Massink, Mieke",
	title="VoxLogicA: A Spatial Model Checker for Declarative Image Analysis",
	booktitle="Tools and Algorithms for the Construction and Analysis of Systems",
	year="2019",
	series="Lecture Notes in Computer Science",
	publisher="Springer International Publishing",
	pages="281--298",
	doi="https://doi.org/10.1007/978-3-030-17462-0_16"
}
```


Quickstart
==========

See the directory "examples/tutorial": in particular the comments in the file "examples/tutorial/tutorial.imgql" 


Using the tool
==============

Releases of VoxLogicA consist of an executable file accompained by a number of
libraries, that must reside in the same directory as the executable.


Command line invocation
-----------------------

First of all, one needs to identify the main executable, residing in the tool
directory. The name of the executable is VoxLogicA on linux and macOS systems,
and VoxLogicA.exe on windows systems. From now on, we will write VoxLogicA to
indicate your executable. Substitute with /path/to/VoxLogicA or
/path/to/VoxLogicA.exe, whatever appropriate.

For normal usage, the executable (VoxLogicA, or VoxLogicA.exe, depending on the
operating system) takes just one parameter, a text file (may have extension
.imgql, but this is not mandatory), containing a description of the analysis to
be executed.
 
**Example** 

    VoxLogicA test.imgql


Inline help and documentation of built-in operators
---------------------------------------------------

To see an help message about command line options, type

    VoxLogicA --help

In particular, you can get a list of all defined built-in operators, with a
short description, by typing

    VoxLogicA --ops

For more information on such operators, please refer to the paper mentioned at
the beginning of this document. 

It is customary to include the VoxLogicA standard library by adding
to an analysis file the following line:

    import "stdlib.imgql"

There is currently no documentation for these (one-line, mostly shorthand) functions, although
most of them are documented in the above research paper. Check the file
"stlib.imgql" in the same directory of the main VoxLogicA executable, and the
comments therein.

Graphical user interface
------------------------

A graphical user interface is being worked on. If you are a skilled electron or
html5 developer and want to contribute, get in touch!


-----------------------------


Analysis file format
--------------------

A VoxLogicA input file is a list of *commands* separated by white
space. Commands specify libraries to be imported, images to be loaded,
constant and function definitions, and images to be saved.

**Example**

    import "stdlib.imgql"

    load x = "images/nii/test3d.nii"
    load y = "images/nii/mask3d.nii"

    let myimage = intensity(x)
    let mymask = intensity(y) >. 0

    save "output/result.nii" mask(myimage,mymask)

---------------------------

Syntax of VoxLogicA analysis files
==================================


Comment syntax
--------------

Comments are introduced using "//". Everything following "//" until
newline is skipped in evaluation.

** Example **

	// This line is ignored


Commands
--------

VoxLogicA supports the following commands: 

- let 		(declarations)
- import 	(import libraries)
- load 		(load images) 
- save 		(save images) 
- print		(print values) 

Declarations (command "let"):
-----------------------------

    let identifier = expression

OR

    let identifier(argument1,argument2,...) = expression

An identifier is 

- either a function or constant identifier, consisting
	of **a sequence of letters or digits starting with a lowercase letter**,

- or an infix or prefix operator identifier, consisting in **a sequence of
letters, digits or symbols, drawn from # ; : _ ' . | ! $ % & / ^ = * -
+ < > ? @ ~ \ .**, starting either with a symbol, or with an uppercase letter.

An expression is a defined numeric or literal constant, or a defined
function applied to the correct number of arguments, or a prefix or
infix operator with its arguments. More than two arguments are
possible for infix operators; see the example below.

**Examples**

Constant declaration

    let x = 3 		
    let y = 3.1

	let a = red(image)	

Function declaration

    let xor(a,b) = and(or(a,b),not(and(a,b)) 	

	// Usage
	let c = xor(a,b)

Prefix and infix operators:

	let !(a) = not(a)
    let <|>(a,b) = xor(a,b)

	// Usage
	let x = ! a
	let y = a <|> b

Infix operator with more than two arguments: 

    let ++(a,b,c,d) = dt(a,b) >. (c+d)

	// Square brackets are used for arguments from the third on;
	// the following is interpreted as ++(a,b,c,d) = dt(a,b) > (c+d)
	let x =  a ++[c,d] b

Library import: command "import"
--------------------------------

	import "filename"

If filename is a relative path (that is, it does not start with "/"), the file
to be imported is first searched in the current directory, then in the directory
where the VoxLogicA executable is. Note that "stdlib.imgql" is bundled with
VoxLogicA.

**Example**

    import "stdlib.imgql"

Image loading (command "load"):
-------------------------------

    load identifier = filename

Identifiers follow the "constant or function" rule, that is, they
consist in a sequence of letters or digits, starting with a lowercase
letter. The identifier is subsequently used to access specific
features of an image.

Supported file formats are ".bmp", ".jpg", ".png", ".nii", and ".nii.gz". 

NOTE: ITK seems to not support some versions of the .bmp file format. If you get
a failure while loading a .bmp image, try converting it to .png beforehand.


**Example**

    load x = "test.png"
    let intsty = intensity(x)
	let r = red(x)

NOTE: VoxLogicA only supports RGB and RGBA color spaces. If you need to use CMYK
images, you can either convert them to RGB, or save them to separate
monochromatic images, and load/save them separately. You can use multiple load
instructions to achieve this.

**Example**

	load c = "img_c.png"
	loat m = "img_m.png"
	load y = "img_y.png"
	load k = "img_k.png"



Image saving: command "save"
----------------------------

	save "filename" expression

**Example**

    load x = "test.png"
    load y = "test2.png"
    let intsty1 = intensity(x)
    let intsty2 = intensity(y)

    save "output.nii" (intsty1 >. 0) & (intsty2 <. 3)

Printing values: command "print"

	print "message" expression

The print command prints to the log file the string provided as message,
followed by an "=" sign and the value of the provided expression.

**Example**

	print "Red area size" volume((red(img)=255) & (green(img)=0) & (blue(img)=0)


Type system
===========

The VoxLogicA type system (quite simple, right now) is described in the research
paper mentioned at the beginning of this document. Voxlogica currently
implements just four types, although it is planned to expand the type system in
the near future, when more operations are added to VoxLogicA.

Basic types:

- number            (no difference between float and int; internally, float32)  
- model				(the type assigned to "x" in load x = "filename")
- valuation(number)	(image with number-valued voxels)
- valuation(bool)   (image with truth values in voxels, a.k.a. region of interest)

---------------------------

Builtin operators
=================

Run the command below to see a list of builtin operators, their types and a short description.

	VoxLogicA --ops

