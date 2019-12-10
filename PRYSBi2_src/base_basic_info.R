base_program = "val_base_program"
platform     = val_platform
mcmc_sim     = val_mcmc_sim
grid_output  = "val_val_grid_output"
project_name = "val_val_project_name"
day_num      = "val_day_num"
ssp_list     = "val_ssp_list"
rcp_list     = "val_rcp_list"
gcm_list     = "val_gcm_list"
croplist     = "val_croplist"
ca_list      = "val_ca_list"
adapt_list   = "val_adapt_list"
iyrstr       = val_iyrstr
iyrend       = val_iyrend

est_dim      = val_est_dim
nchain       = val_nchain
nstep        = val_nstep
ndim         = val_ndim

param_interval = val_param_interval
est_interval   = val_est_interval

ssp_lists   = strsplit(ssp_list,   " ")[[1]]
rcp_lists   = strsplit(rcp_list,   " ")[[1]]
gcm_lists   = strsplit(gcm_list,   " ")[[1]]
croplists   = strsplit(croplist,   " ")[[1]]
ca_lists    = strsplit(ca_list,    " ")[[1]]
adapt_lists = strsplit(adapt_list, " ")[[1]]

if(platform == 1){
  output_dir='/Volumes/G-DRIVE-3T-1/HAS'
}
if(platform == 2){
  output_dir='/lfs/sakuraigen/HAS'
}

if(mcmc_sim == 1) calc_type = "test"
if(mcmc_sim == 2) calc_type = "mcmc"
if(mcmc_sim == 3) calc_type = "sim"

nyr = length(iyrstr:iyrend)

