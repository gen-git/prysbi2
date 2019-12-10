module control_param

implicit none 

integer        :: read_spm_param_namelist ! Read namelist for spm_param (1) or not (0)
character(100) :: prysbi_namelist_file ! Secify the file containing namelist of PRYSB
character(100) :: target_plant ! Target plant
integer        :: common_nsol ! The number of soil layer
integer        :: ctl_adjust_plnt ! the adjusted day from plnt_day (default = 0)
integer        :: ctl_cut_strsw ! Wheather cut strsw (1) or not (0)
integer        :: ctl_nspin ! The number of years for spin up
integer        :: ctl_ca ! Controlling CO2 concentration (for Ag-GRID phase2)
integer        :: ctl_adapt ! Controlling Adaptation option (for Ag-GRID phase2)

namelist /control_list/ read_spm_param_namelist, prysbi_namelist_file, target_plant, common_nsol, &
& ctl_adjust_plnt, ctl_cut_strsw, ctl_nspin, ctl_ca, ctl_adapt

end module
