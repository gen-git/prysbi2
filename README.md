# PRYSBI2 : crop development simulation

"PRYSBI2" enables us to simulate a crop development model (PRYSBI2) in global scale. This program include not only the simulation module but also the module by which users can estimate the probability distribution of parameters of the crop model using MCMC.

## Description

The basic structure is explained in Sakurai et al. (2014). Moreover, though the number of crops that can be simulated has been increased, the difference in the model among crops is basically only in the parameter values. Therefore, the different crops share the basic model structure (see Program_1.2/PRYSBI2/spm_param_fix).  
The paper that includes the model description is now under review. However, you are free to submit the manuscript before the paper is accepted (for example, Okada et al. 2015, Porwollik et al. 2016, Müller et al. 2017).

## Requirement

For execution of the program, you should install Fortran MPI compiler on your PC or the server you use. The operation check was done on mpif90 and ifort.

## Usage

If you choose "val_platform=1" in "all_excute.sh", the makefile of the program using **mpif90** is created, and compiling and execution of the executable file are automatically processed by only inputting `bash all_excute.sh` in console.  
If you choose "val_platform=2" in "all_excute.sh", the makefile of the program using **ifort** is created, and compiling and submission of the job file are automatically processed by only inputting `bash all_excute.sh` in console on your server (PBS is assumed as job management application).  
If you want to change compiler according to your environment, please change the makefile in "/Program1.2/Base_settting" directory ("base_makefile_pc" or "base_makefile_sv").

You can simulate PRYSBI2 by executing "all_excute.sh" in this directory.  
Some of the simulation settings can be changed in this file. The explanations of the variables are described as the comments in the file(Please change from "Setting" to "End setting").
* simulation years
* crop name
* simulation name
* MCMC setting      etc.

## Path of input data

The path of the input files (directories) are specified in "base_control.sh". In this file,
* the path of output directory (val_output_dir_eb and val_output_dir_nb)
* the path of climate data (val_posi_whether)
* the path of parameter directory (val_posi_param)
* the path of other data (data_dir)
can be changed.

When changing path, be sure to add backslash before slash except for "val_output_dir/nb". When specifying the data paths on PC, those should be described below "# Data path 1". When specifying the data paths on server, those should be described below "# Data path 2".

## Input data

The input data should be prepared as raster format except for some data (for example, CO2 data). The basic structure of the format is a vector of length 51200 (320 x 160) with 4 bite. The starting point of the vector is at lat. 90 degrees S., long. 0 degree W. and the end point is at lat. 90 degrees N., long. 0 degree E.. The variables should be arranged to the east. The spatial resolution of the raster data is 1.125 degrees x 1.125 degrees. When a data file includes multiple maps (that is, the file have multiple 320 x 160 vector data), the vectors are simply connected each other in order of time. For example, the climate data of each year have a vector of 320 x 160 x 365 length. We call this format as "grd format" in the following.

### Climate data

The following is the list of climate data needed for the simulation of the crop model.
1. solar radiation (MJ/m2): dswrfsfc****.grd
2. precipitation (mm/day): precsfc****.grd
3. relative humidity (%): rhsfc****.grd
4. average temperature (degree Celsius): tavesfc****.grd
5. daily maximum temperature (degree Celsius): tmaxsfc****.grd
6. daily minimum temperature (degree Celsius): tminsfc****.grd
7. wind speed (m/s): windsfc****.grd

### Parameter values

When you want to change a (some) parameter value(s) for each spatial grid, you have to prepare the data file on the parameter values as grd format. If you simulate multiple ensemble, you have to prepare the data that have a vector of 51200 x the number of ensemble length. The file of the parameter data should be named as "grd_param_list_all_[parameter number]\_[crop name].grd". The parameter value that is read in the program is usually assigned in the subroutine, "parameter_setting" (Program_1.2/MCMC/Mcalc_llk.f90). When you want to change the number of parameters that are changed in each grid (default is four), following constants should be changed in addition to the alteration of the subroutine.
* avar_param (Program_1.2/Reader/var_read_all.f90)
* val_ndim (all_excute.sh)
* mcmc_setting_[crop name].dat (Program_1.2/Base_setting/)

The main role of the file "mcmc_setting_[crop name].dat" is to alter the setting of prior distribution and MCMC. In this file, the mean and S.D. etc. of the prior distribution of the target parameter are altered. Therefore, if you only use the parameter values for simulation, you only have to insert a line(s) as following according to the number of parameters.
	x* -9999 -9999 -9999 -9999 -9999

## MCMC

When you want to estimate the probability distributions of some parameter of the crop model following alteration should be needed. Note that, because tens of thousands of MCMC steps are needed for the convergence of MCMC, I recommend using clustering computing system that have tens of (or hundreds of) cores if you want to estimate parameter values in large area.
1. all_excute.sh
	You have to change multiple constants included in this file according to the comment. Note that the constant "val_nstep" specifies the number of MCMC steps if you set "val_mcmc_sim" as 2. On the other hand, the constant specifies the number of ensembles simulated if you set "val_mcmc_sim" as 3.
	You also have to change "val_ndim" on this file according to your concept.
2. mcmc_setting_[crop name].dat (Program_1.2/Base_setting/)
	You have to set the information of prior distribution of the parameters in this file for each crop. The column "typeprior" defines the type of prior distribution. If you assume a uniform distribution, you have to assign 0 to it, and define the range of the distribution (columns "lim(lw)" and "lim(up)"). If you assume a normal distribution, you have to assign 1 to it, and define the mean and S.D. of the distribution (columns "avprior" and "sdprior"). The constants "dream_b", "dreambdash" and "dream_delta" are detailed settings for MCMC. Therefore, you don't always have to alter them.
3. Mcalc_llk.f90 (Program_1.2/MCMC)
	You have to alter the subroutine "parameter_setting" on this file according to the concept of your parameter setting.
4. avar_param (Program_1.2/Reader/var_read_all.f90)

## Refefences

Sakurai et al. (2014) How much has the increase in atmospheric CO2 directly affected past soybean production? Scientific Reports.
Okada et al. (2015) Modeling irrigation-based climate change adaptation in agriculture: model development and evaluation in Northeast China. Journal of Advances in Modeling Earth Systems.
Müller et al. (2017) Global gridded crop model evaluation: benchmarking, skills, deficiencies and implications. Geoscientific Model Development.
Porwollik et al. (2016) Spatial and temporal uncertainty of crop yield aggregations. European Journal of Zoology.
