VoxLogicA: Voxel-based Logical Analyser
=======================================

VoxLogicA is a tool for analysing images using ImgQL specifications.

ImgQL is a language to analyse images, based on spatial logics. VoxLogicA
interprets ImgQL specifications using a model-checking approach, which entails,
in particular, heavy use of memoisation techniques to permit complex analyses in
short execution times, at the expense of used memory.

VoxLogicA is distributed under a permissive open source license. See the file
LICENSE.txt for details.

This file is meant to be an user manual for the tool. However, one can also
refer to the following research paper, which illustrates the philosophy of the
tool in more detail, with a detailed case study.

https://arxiv.org/abs/1811.05677

NOTE: the above link points to a preprint of an academic paper to be published in 
the proceedings of the "25th International Conference on Tools and Algorithms for 
the Construction and Analysis of Systems (TACAS 2019)".  See:

https://conf.researchr.org/track/etaps-2019/tacas-2019-papers

If you use VoxLogicA in your research, please consider citing the relevant paper(s):

@InProceedings{BCLM2019TACAS,
author="Belmonte, Gina and Ciancia, Vincenzo and Latella, Diego and Massink, Mieke",
title="VoxLogicA: a Spatial Model Checker for Declarative Image Analysis",
booktitle="Tools and Algorithms for the Construction and Analysis of Systems",
year="2019",
publisher="Springer International Publishing",
address="Cham",
pages="To appear",
}



Quickstart
==========

See the directory "example" and the comments in the file "example.imgql" 


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

For completeness, below we report the output of the command

	VoxLogicA --ops

./.(number,number) : number
Floating point division

.*.(number,number) : number
Floating point multiplication

.+.(number,number) : number
Floating point addition

.-.(number,number) : number
Floating point subtraction

crossCorrelation(number,valuation(number),valuation(number),valuation(bool),number,number,number) : valuation(number)
similarity via statistical cross-correlation (see academic papers or extended documentation)

constant(number) : valuation(number)
The image which has the given value at each voxel

.=(number,valuation(number)) : valuation(bool)
eq(n,i) is true at voxels of i that are equal to n

.<=(number,valuation(number)) : valuation(bool)
n .<= img is true at voxels of img that are greater than or equal to n

.>=(number,valuation(number)) : valuation(bool)
n .>= img is true at voxels of img that are less than or equal to n

between(number,number,valuation(number)) : valuation(bool)
between(n1,n2,i) is true at voxels of i that are greater than or equal to n1, and less than or equal to n2

max(valuation(number)) : number
Finds the maximum value among the voxels in its argument

min(valuation(number)) : number
Finds the minimum value among the voxels in its argument

+(valuation(number),valuation(number)) : valuation(number)
Voxel-wise addition

*(valuation(number),valuation(number)) : valuation(number)
Voxel-wise multiplication

-(valuation(number),valuation(number)) : valuation(number)
Voxel-wise subtraction

mask(valuation(number),valuation(bool)) : valuation(number)
mask(img,bimg) has value 0 at voxels that are false in bimg, and the same value of img at voxels that are true in bimg

avg(valuation(number),valuation(bool)) : number
avg(img,bimg) is the average of the values of img at voxels that are true in bimg

/.(valuation(number),number) : valuation(number)
divides each voxel by a constant

-.(valuation(number),number) : valuation(number)
subtracts a constant from each voxel

+.(valuation(number),number) : valuation(number)
adds a constant to each voxel

*.(valuation(number),number) : valuation(number)
multiplies each voxel by a constant

dt(valuation(bool)) : valuation(number)
Euclidean distance transform of its argument: replaces each voxel with the positive (or 0) distance from the nearest voxel which is true in the argument.

near(valuation(bool)) : valuation(bool)
Spatial-logical closure (that is, dilation)

interior(valuation(bool)) : valuation(bool)
Spatial-logical interior (that is, erosion)

through(valuation(bool),valuation(bool)) : valuation(bool)
through(img1,img2) is true at voxel x if there is a path p, starting in x and ending in a voxel y, with y true in img1, and all points of p (including extremes) true in img2

not(valuation(bool)) : valuation(bool)
Boolean negation of each voxel

and(valuation(bool),valuation(bool)) : valuation(bool)
Boolean and voxel-wise

or(valuation(bool),valuation(bool)) : valuation(bool)
Boolean or voxel-wise

intensity(model) : valuation(number)
The intensity  of an image. For RGB images this is computed with the well known colorimetric formula.

red(model) : valuation(number)
The red component of an image. For grayscale images this is equal to the intensity

green(model) : valuation(number)
The green component of an image. For grayscale images this is equal to the intensity

blue(model) : valuation(number)
The blue component of an image. For grayscale images this is equal to the intensity

alpha(model) : valuation(number)
The alpha channel of an image. If there is no alpha channel, a constant image with all voxels equal to 255 is returned

volume(valuation(bool)) : number
The number of voxels that are true in the given image

maxvol(valuation(bool)) : valuation(bool)
The connected component of the given image with maximum volume (if more components have the same maximum volume, their union is returned)

percentiles(valuation(number),valuation(bool)) : valuation(number)
Each voxel in percentiles(img,bimg) is the percentile, between 0 and 1, of its value in img, considering only voxels that are true in bimg (voxels that are false in bimg are assigned value 0)

rgb(valuation(number),valuation(number),valuation(number)) : model
Creates a RGB image given the red, green, and blue components

rgba(valuation(number),valuation(number),valuation(number),valuation(number)) : model
Creates a RGBA image given the red, green, blue, and alpha components

tt : valuation(bool)
The image which is true at each voxel

ff : valuation(bool)
The image which is false at each voxel

border : valuation(bool)
True at voxels in the border of the image

