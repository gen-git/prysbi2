base_program = "Program_1.2"
platform     = 1
mcmc_sim     = 3
grid_output  = "Grid_Output"
project_name = "Distribuo"
day_num      = "2018-06-15"
ssp_list     = "historical"
rcp_list     = "historical"
gcm_list     = "AgMERRA"
croplist     = "maize_major"
ca_list      = "-999"
adapt_list   = "0"
iyrstr       = 1961
iyrend       = 2010

est_dim      = 3
nchain       = 20
nstep        = 1
ndim         = 4

param_interval = 10
est_interval   = 10

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

