VoxLogicA: Voxel Logical Analysis
=================================

ImgQL is a language to analyse images, based on spatial
logics and using a model-checking approach, which entails, in
particular, heavy use of memoisation techniques to permit very complex
analysis in short execution times, at the expense of memory, which is
not considered scarce, also because on-disk caching is integrated in
the approach.

VoxLogicA is a tool for analysing images using ImgQL specifications.


License
=======

See the file LICENSE.txt


Using the tool
==============

Releases of VoxLogicA consist of an executable file with a number of
libraries, that must reside in the same directory of the executable.

Executable file
---------------

The executable (VoxLogicA, or VoxLogicA.exe, depending on the operating
system) takes only one parameter, a file (may have extension .imgql,
but this is not mandatory), containing a description of the analysis
to be executed.
 
[Example] ----------------

/path/to/VoxLogicA test.imgql

---------------------------


File format
-----------

A VoxLogicA input file is a list of commands separated by white
space. Commands specify libraries to be imported, images to be loaded,
constant and function definitions, and images to be saved.

[Example] ----------------

import "stdlib.imgql"

load x = "images/nii/test3d.nii"
load y = "images/nii/mask3d.nii"

let image = intensity(x)
let mymask = intensity(y) > 0

save "output/a.nii" mask(image,mymask)

---------------------------


Comment syntax
--------------

Comments are introduced using "//". Everything following "//" until
newline is skipped in evaluation.


Commands
--------

VoxLogicA supports the following commands

Declarations:

let identifier = expression

OR

let identifier(argument1,argument2,...) = expression

An identifier is either a function or constant identifier, consisting
in a sequence of letters or digits, starting with a lowercase letter,
or an infix or prefix operator identifier, consisting in a sequence of
letters, digits or symbols, drawn from # ; : _ ' . | ! $ % & / ^ = * -
+ < > ? @ ~ \ .

An expression is a defined numeric or literal constant, or a defined
function applied to the correct number of arguments, or a prefix or
infix operator with its arguments. More than two arguments are
possible for infix operators; see the example below.

[Example] ----------------

// Constant
let x = 3 		
let y = 3.1

// Function
let xor(a,b) = and(or(a,b),not(and(a,b)) 	

// Infix operator, use as x <|> y
let <|>(a,b) = xor(a,b)

// Infix operator; arguments beyond the first two are specified after
// the operator in square brackets, e.g. x++[z] y is translated to
++(x,y,z)
let ++(a,b,c) = dt(a,b) > c
								

---------------------------


Library import: import "filename"

[Example] ----------------

import "stdlib.imgql"

---------------------------

Image loading: load identifier = filename

Identifiers follow the "constant or function" rule, that is, they
consist in a sequence of letters or digits, starting with a lowercase
letter. The identifier is subsequently used to access specific
features of an image.

Supported file formats are those of the ITK library, including ".nii",
".nii.gz", ".jpg", ".png".

[Example] ----------------

load x = "test.png"
let intsty = intensity(x)

--------------------------

Image saving: save filename expression

[Example] ----------------

load x = "test.png"
load y = "test2.png"
let intsty1 = intensity(x)
let intsty2 = intensity(y)

save "output.nii" (intsty1 > 0) & (intsty2 < 3)

---------------------------

Builtin operators
-----------------

VoxLogicA types are numbers (floating point), images (returned by load),
or formulas. Formulas can be quantitative (resulting from
e.g. intensity, or distance transforms) or boolean (resulting
e.g. from boolean operators or thresholds). Below we write these as
"qformula" and "formula".

- Boolean operators: 
	tt : formula,
	ff: formula,
	and(formula,formula) : formula,
	or(formula,formula) : formula,
	not(formula) : formula

- Spatial operators: 
	near(formula) : formula,
	interior(formula) : formula,
	flood(formula,formula) : formula

	flood(a,b) is true at all points that are reachable, passing
	only by points satisfying a or b, from a point satisfying
	a. This is useful to define surrounded, reach, touch as
	derived operators.

- Quantitative operators (thresholds and similar): 
	eq(number,qformula) : formula,
	geq(number,qformula) : formula,
	leq(number,qformula) : formula, 
	between(number,number,qformula) : formula,
	max(qformula) : number,
	min(qformula) : number, 
	subtract(qformula,qformula) : qformula,
	mask(qformula,formula) : qformula

	The "mask" operator may be non-obvious. mask(qf,f) returns a
	quantitative formula which has value 0 at points where f is
	false, and the same value of qf at points where f is true.

- Distance transform: 
	dt(formula) : qformula

	dt(f) returns the Euclidean distance map of f, that is, a
	quantitative formula holding at each point the minimum
	distance from that point to a point where f is true.
	Distances are positives at points where f is false ("outside"
	f), negative at points where f is true ("inside" f).

- Statistical operators:
	crossCorrelation(number,qformula,qformula,formula,number,number,number) : qformula

	crossCorrelation(rho,qfa,qfb,f,min,max,nbins) computes at each
	point x the cross-correlation of the histogram of the values
	of qfa on the hyperrectangle of radius rho, with the histogram
	of the values of qfb on the points of the image satisfying
	f. The parameters min, max and nbins determines which values
	of qfa and qfb are taken into account, and how many bins are
	used.

- Image operators:
	intensity(image) : qformula

- Border operator:
	border : formula


Derived operators:
------------------

See stdlib.imgql


COMPILING
=========

install .net core (portable) 2.1, cd to the "src" subdirectory of the repository type "make release-OS" where OS is one of linux-x64, osx-x64, win-x64. Find your executables in the "releases" subdirectory of the repository. If you don't have make, read the Makefile; dotnet build IS supported for compiling.
