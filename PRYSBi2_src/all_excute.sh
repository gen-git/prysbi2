#!/bin/sh
######################
####### Setting ######
######################
base_program="Program_1.2" # Program name
val_val_grid_output="Grid_Output" # The name of output folder
val_platform=1 # 1: PC, 2: Linux server
val_mcmc_sim=3 # 1: Test, 2: MCMC, 3: Simulation
val_day_num="2018-06-15" # Date of the simulation
val_val_project_name="Distribuo" # Project name
val_param_set="Param_Set_L3" # The name of parameter set (needed if val_mcmc_sim == 3)
val_ssp_list="historical" # The sub-directory name of GDP (historical, SSP1, SSP2, SSP3, Constant2000)
val_rcp_list="historical" # The sub-directory name of climate data (historical, RCP26, RCP45, RCP60, RCP85)
val_gcm_list="AgMERRA" # The name of climate data
val_croplist="maize_major" # Crop name
val_ca_list="-999" # CO2 concentration (-999 when not fixing and using historical data)
val_adapt_list="0" # Not adapt (0), adapt (1)
val_iyrstr=1961 # Start year
val_iyrend=2010 # End year
val_tgt_grid="maize_major_10yr_1980-2010_exist_hvst.grd" # File name in Target_grid directory
val_tgt_change_per_crop=0 # Change tgt_grid for each crop (1) or not (0)
val_dir_GDP="GDP_IIASA" # The directory name of GDP
val_dir_pheno="Phenology_2000_Sacks_Fill" # The directory name of planting date
val_val_tgt_version="FAO_2017" # The version of yield data
strs_fac_by=1 # 1: calc by year, 2: calc by GDP diff, 3: calc by fixed value (2006),
              # 4: calc by only GDP (not using estimated param for each grid)
              # notice! if you use (2) you should simulate from 2006.
val_weather_crop=0 # set 1 if using SINTEX-F folder set
#--------------------

#----- nuber of estimation -----
val_est_dim=3 # Dimention of estimation
#-------------------------------

#----- number of chains -----
val_nchain=20 # Please set the value more than two
#----------------------------

#----- number of steps -----
val_nstep=1 # The number of steps calculated
#---------------------------

#----- Setting for MCMC only -----
val_nburn=2000 # The number of MCMC steps treated as burn-in steps.
val_minstep=2000 # The number of minimum MCMC steps (val_dream_nburn + val_dream_minstep)
val_param_interval=10 # The interval parameter saving.
val_est_interval=10 # The interval estimation saving (should be equal to est_interval for Vastigo).
val_gelman_interval=4000 # The interval in which Gelman - Rubin's index is calcualted.
val_ndim=4 # The dimension of parameter estimated.
#---------------------------------

#----- Setting for mpi -----
val_val_select=8
val_val_ncpus=16
val_val_size="large" # small, large, medium-b
#---------------------------

#----- Others -----
val_val_ctl_cut_strsw=1 # consider water stress (1) or not (0)
#------------------

########################
###### End setting #####
########################

# make basic info.R
sed -e    "s/val_platform/$val_platform/g" "base_basic_info.R" >    "basic_info_"$val_day_num".R"
sed -i -e "s/val_mcmc_sim/$val_mcmc_sim/g"                          "basic_info_"$val_day_num".R"
sed -i -e "s/val_val_grid_output/$val_val_grid_output/g"            "basic_info_"$val_day_num".R"
sed -i -e "s/val_val_project_name/$val_val_project_name/g"          "basic_info_"$val_day_num".R"
sed -i -e "s/val_day_num/$val_day_num/g"                            "basic_info_"$val_day_num".R"
sed -i -e "s/val_ssp_list/$val_ssp_list/g"                          "basic_info_"$val_day_num".R"
sed -i -e "s/val_rcp_list/$val_rcp_list/g"                          "basic_info_"$val_day_num".R"
sed -i -e "s/val_gcm_list/$val_gcm_list/g"                          "basic_info_"$val_day_num".R"
sed -i -e "s/val_croplist/$val_croplist/g"                          "basic_info_"$val_day_num".R"
sed -i -e "s/val_ca_list/$val_ca_list/g"                            "basic_info_"$val_day_num".R"
sed -i -e "s/val_adapt_list/$val_adapt_list/g"                      "basic_info_"$val_day_num".R"
sed -i -e "s/val_iyrstr/$val_iyrstr/g"                              "basic_info_"$val_day_num".R"
sed -i -e "s/val_iyrend/$val_iyrend/g"                              "basic_info_"$val_day_num".R"
sed -i -e "s/val_est_dim/$val_est_dim/g"                            "basic_info_"$val_day_num".R"
sed -i -e "s/val_nchain/$val_nchain/g"                              "basic_info_"$val_day_num".R"
sed -i -e "s/val_nstep/$val_nstep/g"                                "basic_info_"$val_day_num".R"
sed -i -e "s/val_ndim/$val_ndim/g"                                  "basic_info_"$val_day_num".R"
sed -i -e "s/val_base_program/$base_program/g"                      "basic_info_"$val_day_num".R"
sed -i -e "s/val_param_interval/$val_param_interval/g"              "basic_info_"$val_day_num".R"
sed -i -e "s/val_est_interval/$val_est_interval/g"                  "basic_info_"$val_day_num".R"

if [ -e *-e ]
then
  rm *-e
fi

start_id=1
val_start_enumber=$start_id
for igcm in $val_gcm_list
do
  for issp in $val_ssp_list
  do
    for ircp in $val_rcp_list
    do
      for ica in $val_ca_list
      do
        for iadapt in $val_adapt_list
        do
        if [ $val_mcmc_sim = 1 ]; then
          val_out_version=$val_day_num"_for_test_"$ircp"_"$issp"_"$igcm"_c"$ica"_A"$iadapt
        fi
        if [ $val_mcmc_sim = 2 ]; then
          val_out_version=$val_day_num"_for_mcmc_"$ircp"_"$issp"_"$igcm"_c"$ica"_A"$iadapt
        fi
        if [ $val_mcmc_sim = 3 ]; then
          val_out_version=$val_day_num"_for_sim_"$ircp"_"$issp"_"$igcm"_c"$ica"_A"$iadapt
        fi
        val_val_program_version="_Temp_"$val_out_version
        temp_val_iyrend=$val_iyrend

        val_est_n=`expr $temp_val_iyrend - $val_iyrstr + 1`

        rm -R -f "Program"$val_val_program_version
        cp -R $base_program "Program"$val_val_program_version

        sed -e    "s/val_out_version/$val_out_version/g" "base_control.sh" > "control_"$val_out_version".sh"
        sed -i -e "s/val_rcp/$ircp/g"                                        "control_"$val_out_version".sh"
        sed -i -e "s/val_ssp/$issp/g"                                        "control_"$val_out_version".sh"
        sed -i -e "s/val_gcm/$igcm/g"                                        "control_"$val_out_version".sh"
        sed -i -e "s/val_param_set/$val_param_set/g"                         "control_"$val_out_version".sh"
        sed -i -e "s/val_iyrstr/$val_iyrstr/g"                               "control_"$val_out_version".sh"
        sed -i -e "s/val_iyrend/$temp_val_iyrend/g"                          "control_"$val_out_version".sh"
        sed -i -e "s/val_est_n/$val_est_n/g"                                 "control_"$val_out_version".sh"
        sed -i -e "s/val_val_program_version/$val_val_program_version/g"     "control_"$val_out_version".sh"
        sed -i -e "s/val_start_enumber/$val_start_enumber/g"                 "control_"$val_out_version".sh"
        sed -i -e "s/val_platform/$val_platform/g"                           "control_"$val_out_version".sh"
        sed -i -e "s/val_croplist/$val_croplist/g"                           "control_"$val_out_version".sh"
        sed -i -e "s/val_tgt_grid/$val_tgt_grid/g"                           "control_"$val_out_version".sh"
        sed -i -e "s/val_tgt_change_per_crop/$val_tgt_change_per_crop/g"     "control_"$val_out_version".sh"
        sed -i -e "s/val_param_set/$val_param_set/g"                         "control_"$val_out_version".sh"
        sed -i -e "s/val_nstep/$val_nstep/g"                                 "control_"$val_out_version".sh"
        sed -i -e "s/val_nchain/$val_nchain/g"                               "control_"$val_out_version".sh"
        sed -i -e "s/val_est_dim/$val_est_dim/g"                             "control_"$val_out_version".sh"
        sed -i -e "s/strs_fac_by/$strs_fac_by/g"                             "control_"$val_out_version".sh"
        sed -i -e "s/val_weather_crop/$val_weather_crop/g"                   "control_"$val_out_version".sh"
        sed -i -e "s/val_val_project_name/$val_val_project_name/g"           "control_"$val_out_version".sh"
        sed -i -e "s/val_val_ctl_ca/$ica/g"                                  "control_"$val_out_version".sh"
        sed -i -e "s/val_val_ctl_adapt/$iadapt/g"                            "control_"$val_out_version".sh"
        sed -i -e "s/val_dir_GDP/$val_dir_GDP/g"                             "control_"$val_out_version".sh"
        sed -i -e "s/val_dir_pheno/$val_dir_pheno/g"                         "control_"$val_out_version".sh"
        sed -i -e "s/val_mcmc_sim/$val_mcmc_sim/g"                           "control_"$val_out_version".sh"
        sed -i -e "s/val_go_stop/$val_go_stop/g"                             "control_"$val_out_version".sh"
        sed -i -e "s/val_nburn/$val_nburn/g"                                 "control_"$val_out_version".sh"
        sed -i -e "s/val_minstep/$val_minstep/g"                             "control_"$val_out_version".sh"
        sed -i -e "s/val_param_interval/$val_param_interval/g"               "control_"$val_out_version".sh"
        sed -i -e "s/val_est_interval/$val_est_interval/g"                   "control_"$val_out_version".sh"
        sed -i -e "s/val_gelman_interval/$val_gelman_interval/g"             "control_"$val_out_version".sh"
        sed -i -e "s/val_ndim/$val_ndim/g"                                   "control_"$val_out_version".sh"
        sed -i -e "s/val_val_grid_output/$val_val_grid_output/g"             "control_"$val_out_version".sh"
        sed -i -e "s/val_val_select/$val_val_select/g"                       "control_"$val_out_version".sh"
        sed -i -e "s/val_val_ncpus/$val_val_ncpus/g"                         "control_"$val_out_version".sh"
        sed -i -e "s/val_val_size/$val_val_size/g"                           "control_"$val_out_version".sh"
        sed -i -e "s/val_val_ctl_cut_strsw/$val_val_ctl_cut_strsw/g"         "control_"$val_out_version".sh"
        sed -i -e "s/val_val_tgt_version/$val_val_tgt_version/g"             "control_"$val_out_version".sh"

        if [ -e *-e ]
        then
          rm *-e
        fi

        # make and submit
        bash "control_"$val_out_version".sh"

        val_start_enumber=`expr $val_start_enumber + 1`

        done
      done
    done
  done
done
