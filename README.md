# most
Model of Seed Transfer

Directories content
===================
- main directory : *.f90 source files, scripts, compiled and linked program
  - --------> cdflib library : see below
  - --------> modules : compiled *.mod files
  - --------> inputdata : input data files read by most execution
  - --------> outputdata : output data files with results of calculations


COMPILATION
===========
Computational resources have been provided by the Consortium des Équipements de Calcul Intensif (CÉCI), funded by the Fonds de la Recherche Scientifique de Belgique (F.R.S.-FNRS) under Grant No. 2.5020.11 and by the Walloon Region : http://www.ceci-hpc.be/
The program was developed on Ceci Linux computer cluster of ULiège using Intel Fortan v14 compiler. Newer versions of the compiler should work but have not been tested.

https://software.intel.com/content/www/us/en/develop/tools/compilers/fortran-compilers.html



CDFLIB Probability library
--------------------------
CDFLIB, a FORTRAN90 code which evaluates the cumulative density function (CDF) associated with common probability distributions, by Barry Brown, James Lovato, Kathy Russell.
Downloaded from https://people.sc.fsu.edu/~jburkardt/f_src/cdflib/cdflib.html

To create the **cdflib.a** library execute the script **./create_cdflib.sh** in the cdflib directory.
		

Main program compilation
------------------------
The cdflib.a file must exist in cdflib subdirectory.
The list of source files to be compiled is contained in **sourcesall.dat** file.

Execute the bash script **./compile.sh** in the main directory.

The executable **most.out** will be created.

Main program execution
----------------------
Calculations parameters are located in **parameters.dat** file

- ./run.sh  calculates the month defined in parameters.dat
- ./run_all_months.sh calculates 12 months from jan. to dec.

MODULES
-------
- most.f90	      : main program
- mod_common.f90      : declaration of global variables
- mod_parameters.f90	: Reads values in parameter.dat and store them in common variables declared in mod_common.f90
- mod_groups.90		: Calculates and store an array of n weeks . Called by main program.
- mod_week.f90        : A week contains an array of 7 paths (days). Calls defecations routines to calculate and store defecation and spitted points.
- mod_path.f90		: object t_path contains an array of points for one day from wake-up to bed
- mod_geo.f90         : Geometric calculations
- mod_nest.f90		: sleeping sites (or nests) management
- mod_faidef.f90      : reads an array of 12 FAI_DEF values, one for each month. FAI_DEF is used in state calculations
- mod_daytime.f90     : reads an array of 12 month with mean SD values for normal distribution to calculate wake-up bed times
- mod_angle.f90       : calculates new angle following Von Mises distribution. Provides new angles to mod_point module.
- mod_step.f90		: calculates new steps following Gamma distribution. Provides new steps to mod_point module.
- mod_point.f90		: A point is the location where the monkey stops after one step. The module calculates points following random angles and step length and states.
- mod_pointpointer.f90: t_pointpointer objects contain point a index and various methods to manipulates points
- mod_trees.f90       : Module stores feeding areas read in input files depending on month. Used for defecation and points calculations
- mod_state.f90		: routines to calculate randomly new states used to create points
- mod_defecation.f90	: t_defecation class to store results of calculations made in mod_week.
- mod_transit.f90     : create randomized transit time following normal distribution used by defecation calculations
- mod_tools.f90       : utility functions to manipulates strings and file names 

