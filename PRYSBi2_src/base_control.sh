#!/bin/sh
# Platform
platform=val_platform # PC(1), MAFFIN(2)
if [ $platform -eq 1 ]
then
  # Data path 1
  val_output_dir_eb='..\/..\/..\/HAS'
  val_output_dir_nb='../../HAS'
  val_posi_whether='..\/..\/..\/HAS\/Data\/Meteorological_Data'
  val_posi_param='..\/..\/..\/HAS\/Data\/Input_Param'
  data_dir='..\/..\/..\/Data'
fi
if [ $platform -eq 2 ]
then
  # Data path 2
  val_output_dir_eb='\/lfs\/sakuraigen\/HAS'
  val_output_dir_nb='/lfs/sakuraigen/HAS'

  val_posi_whether='\/lfs\/sakuraigen\/HAS\/Data\/Meteorological_Data'
  val_posi_param='\/lfs\/sakuraigen\/HAS\/Data\/Input_Param'
  data_dir='..\/..\/..\/Data'
fi
# Directory
val_grid_output="val_val_grid_output"
# Simulation version
val_dream_simulate_version=val_mcmc_sim # 1: Test, 2: MCMC, 3: Simulation
val_dream_strs_fac_type=strs_fac_by # 1: calc by year, 2: calc by GDP, 3: constant, 4: GDP only
# Project number
project_name="val_val_project_name"
# Version
program_version="val_val_program_version" # The version of program
out_version="val_out_version" # Directory name for output
croplist="val_croplist"
weather_crop=val_weather_crop
# Excute number
start_enumber=val_start_enumber
# Check submit situation (1) or not (2)
go_stop=val_go_stop
# Target year
iyrstt=val_iyrstr
iyrend=val_iyrend
# Weather path
val_path_weather=$val_posi_whether'\/val_gcm\/val_rcp'
val_path_param=$val_posi_param'\/val_param_set'
val_path_fix_param='..\/Fixed_Param'
# Data path
val_path_topo=$data_dir'\/HAS_1.125x1.125_global_grid_datasets\/geo_info'
val_path_soil=$data_dir'\/HAS_1.125x1.125_global_grid_datasets\/soil_ISLSCIP2'
val_path_yield=$data_dir'\/HAS_1.125x1.125_global_grid_datasets\/global_historical_crop_yields'
val_path_pheno=$data_dir'\/HAS_1.125x1.125_global_grid_datasets\/val_dir_pheno'
val_path_co2=$data_dir'\/CO2\/val_rcp'
val_path_gdp=$data_dir'\/HAS_1.125x1.125_global_grid_datasets\/GDP_IIASA\/val_ssp'
val_tgt_version='val_val_tgt_version'
# MCMC namelist
val_dream_nstep=val_nstep # The maximum number of MCMC steps or the number of ensembles.
val_dream_nburn=val_nburn # The number of MCMC steps treated as burn-in steps.
val_dream_minstep=val_minstep # The number of minimum MCMC steps.
val_dream_param_interval=val_param_interval # The interval parameter saving.
val_dream_est_interval=val_est_interval # The interval estimation saving.
val_dream_gelman_interval=val_gelman_interval # The interval in which Gelman - Rubin's index is calcualted.
val_dream_ncr=5 # The number of chains crossed at each chain step.
val_dream_est_dim=val_est_dim # The dimension of estimation which is outputed.
val_dream_est_n=val_est_n # The length of estimation outputed. The length is euqal to the length of years.
val_dream_nchain=val_nchain # The number of MCMC chains.
val_dream_ndim=val_ndim # The dimension of parameter estimated.
# Submision setting
val_select=val_val_select
val_ncpus=val_val_ncpus
val_size=val_val_size
# Change control file
val_ctl_adjust_plnt=0 # the adjusted day from plnt_day (default = 0)
val_ctl_cut_strsw=val_val_ctl_cut_strsw # Wheather cut strsw (1) or not (0)
val_ctl_nspin=3
val_ctl_ca=val_val_ctl_ca # CO2 concentration (for Ag-GRID phase2)
val_ctl_adapt=val_val_ctl_adapt # Adaptation or not (for Ag-GRID phase2)
# Clean
sed -e "s/xxxxx/$program_version/" base_clean.sh > clean.sh
bash clean.sh
rm clean.sh

# Others
val_ctl_cut_strsw=val_val_ctl_cut_strsw
#---Excute-----------------------------------------------------------------------------------------------------
# Make directory for Output
mkdir -p $val_output_dir_nb/$val_grid_output
mkdir -p $val_output_dir_nb/$val_grid_output/$project_name
mkdir -p $val_output_dir_nb/$val_grid_output/$project_name/Output$out_version
# Make dream_namelist.txt
(
cd "Program$program_version"
sed -e    "s/val_dream_nstep/$val_dream_nstep/" Base_setting/base_dream_namelist.txt > Namelist/dream_namelist.txt
sed -i -e "s/val_dream_nburn/$val_dream_nburn/"                                        Namelist/dream_namelist.txt
sed -i -e "s/val_dream_minstep/$val_dream_minstep/"                                    Namelist/dream_namelist.txt
sed -i -e "s/val_dream_param_interval/$val_dream_param_interval/"                      Namelist/dream_namelist.txt
sed -i -e "s/val_dream_est_interval/$val_dream_est_interval/"                          Namelist/dream_namelist.txt
sed -i -e "s/val_dream_gelman_interval/$val_dream_gelman_interval/"                    Namelist/dream_namelist.txt
sed -i -e "s/val_dream_ncr/$val_dream_ncr/"                                            Namelist/dream_namelist.txt
sed -i -e "s/val_dream_est_dim/$val_dream_est_dim/"                                    Namelist/dream_namelist.txt
sed -i -e "s/val_dream_est_n/$val_dream_est_n/"                                        Namelist/dream_namelist.txt
sed -i -e "s/val_dream_nchain/$val_dream_nchain/"                                      Namelist/dream_namelist.txt
sed -i -e "s/val_dream_ndim/$val_dream_ndim/"                                          Namelist/dream_namelist.txt
# Delete unnecessary files
rm -f Namelist/*-e
)
# Make excute file and submit file for each crop
enumber=$start_enumber
for icrop in $croplist
do
(
  if [ $weather_crop -eq 1 ]
  then
    val_path_weather=$val_posi_whether'\/val_gcm\/'$icrop
    val_path_co2=$data_dir'\/CO2\/historical'
  fi
  # Target grid path
  tgt_change_per_crop=val_tgt_change_per_crop
  if [ $tgt_change_per_crop == 0 ]
  then
    val_path_ana='..\/Target_grid\/val_tgt_grid'
  fi
  if [ $tgt_change_per_crop == 1 ]
  then
    val_path_ana='..\/Target_grid\/'$icrop'val_tgt_grid'
  fi
  # Make directory for output
  mkdir -p $val_output_dir_nb/$val_grid_output/$project_name/Output$out_version/$icrop
  cd "Program$program_version"
  # Make read_all_data_$icrop.f90
  sed -e "s/cropcrop/$icrop/g" Reader/base_read_all_data.f90 > Reader/read_all_data_$icrop.f90
  # Make Mread_dreamlist_$icrop.f90
  sed -e "s/cropcrop/$icrop/g" MCMC/base_Mread_dreamlist.f90 > MCMC/Mread_dreamlist_$icrop.f90
  # Make Mread_setting_$icrop.f90
  sed -e "s/cropcrop/$icrop/g" MCMC/base_Mread_setting.f90 > MCMC/Mread_setting_$icrop.f90
  # Make Mdream_param.f90
  sed -e "s/val_dream_nstep/$val_dream_nstep/g" MCMC/base_Mdream_param.f90 > MCMC/Mdream_param.f90
  sed -i -e "s/val_dream_nchain/$val_dream_nchain/g"                         MCMC/Mdream_param.f90
  sed -i -e "s/val_dream_ndim/$val_dream_ndim/g"                             MCMC/Mdream_param.f90
  sed -i -e "s/val_dream_simulate_version/$val_dream_simulate_version/g"     MCMC/Mdream_param.f90
  sed -i -e "s/val_dream_strs_fac_type/$val_dream_strs_fac_type/g"           MCMC/Mdream_param.f90
  # Make submit file for scion
  sed -e "s/cropcrop/$icrop/g" Base_setting/base_submit_mcmc_scion.sh > submit_mcmc_scion_$icrop.sh
  sed -i -e "s/val_project_name/$project_name/"                         submit_mcmc_scion_$icrop.sh
  sed -i -e "s/val_select/$val_select/"                                 submit_mcmc_scion_$icrop.sh
  sed -i -e "s/val_ncpus/$val_ncpus/"                                   submit_mcmc_scion_$icrop.sh
  sed -i -e "s/enumber/$enumber/g"                                      submit_mcmc_scion_$icrop.sh
  sed -i -e "s/val_program_version/$program_version/"                   submit_mcmc_scion_$icrop.sh
  sed -i -e "s/val_size/$val_size/"                                     submit_mcmc_scion_$icrop.sh
  # Make control_namelist_$icrop.txt
  sed -e "s/cropcrop/$icrop/g" Base_setting/base_control_namelist.txt > "Namelist/control_namelist_"$icrop".txt"
  sed -i -e "s/val_ctl_adjust_plnt/$val_ctl_adjust_plnt/"               "Namelist/control_namelist_"$icrop".txt"
  sed -i -e "s/val_ctl_cut_strsw/$val_ctl_cut_strsw/"                   "Namelist/control_namelist_"$icrop".txt"
  sed -i -e "s/val_ctl_nspin/$val_ctl_nspin/"                           "Namelist/control_namelist_"$icrop".txt"
  sed -i -e "s/val_ctl_ca/$val_ctl_ca/"                                 "Namelist/control_namelist_"$icrop".txt"
  sed -i -e "s/val_ctl_adapt/$val_ctl_adapt/"                           "Namelist/control_namelist_"$icrop".txt"
  # Make setting_namelist_$icrop.txt
  sed -e "s/cropcrop/$icrop/g" Base_setting/base_setting_namelist.txt > "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/temp_iyrstt/$iyrstt/g"                                   "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/temp_iyrend/$iyrend/g"                                   "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_weather/$val_path_weather/g"                    "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_ana/$val_path_ana/"                             "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_topo/$val_path_topo/"                           "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_soil/$val_path_soil/"                           "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_yield/$val_path_yield/"                         "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_pheno/$val_path_pheno/"                         "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_co2/$val_path_co2/"                             "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_gdp/$val_path_gdp/"                             "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_tgt_version/$val_tgt_version/"                       "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_nensemble/$val_dream_nstep/"                         "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_param/$val_path_param/g"                        "Namelist/setting_namelist_"$icrop".txt"
  sed -i -e "s/val_path_fix_param/$val_path_fix_param/"                 "Namelist/setting_namelist_"$icrop".txt"
  # Make mcmc_setting_$icrop.dat
  sed -e "s/project_name/$project_name/g" Base_setting/mcmc_setting_$icrop.dat > "mcmc_setting_"$icrop".dat"
  sed -i -e "s/val_output_dir/$val_output_dir_eb/g"                              "mcmc_setting_"$icrop".dat"
  sed -i -e "s/out_version/$out_version/g"                                       "mcmc_setting_"$icrop".dat"
  sed -i -e "s/cropcrop/$icrop/g"                                                "mcmc_setting_"$icrop".dat"
  sed -i -e "s/eee/e$enumber/g"                                                  "mcmc_setting_"$icrop".dat"
  sed -i -e "s/val_grid_output/$val_grid_output/g"                               "mcmc_setting_"$icrop".dat"
  # Make makefile
  if [ $platform -gt 1 ]
  then
  sed -e "s/cropcrop/$icrop/g" Base_setting/base_makefile_sv > makefile_$icrop
  fi
  if [ $platform -eq 1 ]
  then
  sed -e "s/cropcrop/$icrop/g" Base_setting/base_makefile_pc > makefile_$icrop
  fi

  # Make
  make -f makefile_$icrop

  # Submit scion version
  if [ $platform -eq 2 ]
  then
    # Submit
    qsub ./submit_mcmc_scion_$icrop.sh
  fi

  # If excuted on PC
  if [ $platform -eq 1 ]
  then
    mpiexec -n 2 ./main_mcmc_$icrop.out
  fi

  # Delete unnecessary files
  rm -f Reader/*-e
  rm -f MCMC/*-e
  rm -f Namelist/*-e
  rm -f *-e

)
done
