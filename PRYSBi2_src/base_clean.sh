#!/bin/sh
#---Setting---------------------------------------------------------------------------
# Version
program_version="xxxxx" # The version of program
# Crop name calculated: "maize", "rice", "soybean", "wheat_winter", or "wheat_spring"
croplist="maize_major rice_major soybean wheat_spring wheat_winter"
#---Excute----------------------------------------------------------------------------
# Delete dream_namelist.txt
(
cd "Program$program_version"
rm -f Namelist/dream_namelist.txt
# Delete unnecessary files
rm -f Namelist/*-e
make clean
)
# Delete excute file and submit file for each crop
for icrop in $croplist
do
(
	cd "Program$program_version"
  rm -f Reader/read_all_data_$icrop.f90
  rm -f MCMC/Mread_dreamlist_$icrop.f90
  rm -f MCMC/Mread_setting_$icrop.f90
	rm -f MCMC/Mdream_param.f90
	rm -f submit_mcmc_$icrop.sh
	rm -f "Namelist/control_namelist_"$icrop".txt"
	rm -f "Namelist/setting_namelist_"$icrop".txt"
	rm -f "mcmc_setting_"$icrop".dat"
	rm -f makefile_$icrop
  rm -f submit_mcmc_*_$icrop.sh
  # Delete unnecessary files
  rm -f Reader/*-e
  rm -f MCMC/*-e
  rm -f Namelist/*-e
  rm -f *-e
)
done
