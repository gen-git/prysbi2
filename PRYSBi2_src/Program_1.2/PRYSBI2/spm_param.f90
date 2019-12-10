module spm_param

implicit none

real(8) :: init_bio
real(8) :: l_emerge
real(8) :: tmbs
real(8) :: tgdd
real(8) :: max_rd
real(8) :: rdp
real(8) :: clf_ratio1
real(8) :: clf_ratio2
real(8) :: clf_ratio3
real(8) :: sla
real(8) :: sla_change
real(8) :: laimx1
real(8) :: laimx2
real(8) :: frgrw1
real(8) :: frgrw2
real(8) :: cht_max
real(8) :: rd_m_root_rate
real(8) :: rd_m_shoot_rate
real(8) :: trd_base
real(8) :: trd_root1
real(8) :: trd_root2
real(8) :: trd_shoot1
real(8) :: trd_shoot2
real(8) :: c_rate
real(8) :: b_dash
real(8) :: mm
real(8) :: kapa
real(8) :: rd_g_all_rate
real(8) :: basehimx
real(8) :: yield_correct
real(8) :: cold_heat_coef1
real(8) :: cold_heat_coef2
real(8) :: cold_fphu_av
real(8) :: cold_fphu_sd
real(8) :: ic_himx
real(8) :: c1
real(8) :: phi
real(8) :: kmc25
real(8) :: kmo25
real(8) :: ekmc
real(8) :: ekmo
real(8) :: evcmax
real(8) :: dvcmax
real(8) :: svcmax
real(8) :: rd25_fac
real(8) :: erd
real(8) :: ejmax
real(8) :: djmax
real(8) :: sjmax
real(8) :: cup_cfj
real(8) :: vtop
real(8) :: leaf_w
real(8) :: kw
real(8) :: ag_resid_rate
real(8) :: co2_dr_vcmax25
real(8) :: co2_dr_jmax25
real(8) :: co2_dr_sla
real(8) :: co2_dr_rd25_fac
real(8) :: q_base
real(8) :: fgdd_correct
integer :: lai_growth_type  ! Leaf ratio linearly decreases (1) or decreases in sigmodal type (2)
integer :: cold_heat_type   ! Consider cold stress type 1 (1), type 2 (2), or not (0)
integer :: hi_change_type   ! HI changes with year (1) or not change (0)
integer :: base_year        ! Base year for the calculation of HI change with year
integer :: cons_swatnut     ! Calculate swatnut subroutine (1) or not (2)
integer :: cons_rothc       ! Calculate RothC model (1) or not (2)
integer :: cons_strsn       ! Consider nitrogen stress of SWAT type (1) or not (0)

integer :: nsol
integer :: path
real(8) :: adjust_vcmax25
real(8) :: adjust_jmax25
real(8) :: adjust_sla
real(8) :: adjust_rd25_fac
real(8) :: adjust_np_stress
real(8) :: adjust_e
real(8) :: ms_ext_coef

integer :: hvst_lt_plnt

namelist /spm_namelist/ init_bio
namelist /spm_namelist/ l_emerge
namelist /spm_namelist/ tmbs
namelist /spm_namelist/ tgdd
namelist /spm_namelist/ max_rd
namelist /spm_namelist/ rdp
namelist /spm_namelist/ clf_ratio1
namelist /spm_namelist/ clf_ratio2
namelist /spm_namelist/ clf_ratio3
namelist /spm_namelist/ sla
namelist /spm_namelist/ sla_change
namelist /spm_namelist/ laimx1
namelist /spm_namelist/ laimx2
namelist /spm_namelist/ frgrw1
namelist /spm_namelist/ frgrw2
namelist /spm_namelist/ cht_max
namelist /spm_namelist/ rd_m_root_rate
namelist /spm_namelist/ rd_m_shoot_rate
namelist /spm_namelist/ trd_base
namelist /spm_namelist/ trd_root1
namelist /spm_namelist/ trd_root2
namelist /spm_namelist/ trd_shoot1
namelist /spm_namelist/ trd_shoot2
namelist /spm_namelist/ c_rate
namelist /spm_namelist/ b_dash
namelist /spm_namelist/ mm
namelist /spm_namelist/ kapa
namelist /spm_namelist/ rd_g_all_rate
namelist /spm_namelist/ basehimx
namelist /spm_namelist/ yield_correct
namelist /spm_namelist/ cold_heat_coef1
namelist /spm_namelist/ cold_heat_coef2
namelist /spm_namelist/ cold_fphu_av
namelist /spm_namelist/ cold_fphu_sd
namelist /spm_namelist/ ic_himx
namelist /spm_namelist/ c1
namelist /spm_namelist/ phi
namelist /spm_namelist/ kmc25
namelist /spm_namelist/ kmo25
namelist /spm_namelist/ ekmc
namelist /spm_namelist/ ekmo
namelist /spm_namelist/ evcmax
namelist /spm_namelist/ dvcmax
namelist /spm_namelist/ svcmax
namelist /spm_namelist/ rd25_fac
namelist /spm_namelist/ erd
namelist /spm_namelist/ ejmax
namelist /spm_namelist/ djmax
namelist /spm_namelist/ sjmax
namelist /spm_namelist/ cup_cfj
namelist /spm_namelist/ vtop
namelist /spm_namelist/ leaf_w
namelist /spm_namelist/ kw
namelist /spm_namelist/ ag_resid_rate
namelist /spm_namelist/ lai_growth_type  ! Leaf ratio linearly decreases (1) or decreases in sigmodal type (2)
namelist /spm_namelist/ cold_heat_type   ! Consider cold stress type 1 (1), type 2 (2), or not (0)
namelist /spm_namelist/ hi_change_type   ! HI changes with year (1) or not change (0)
namelist /spm_namelist/ base_year        ! Base year for the calculation of HI change with year
namelist /spm_namelist/ cons_swatnut     ! Calculate swatnut subroutine (1) or not (2)
namelist /spm_namelist/ cons_rothc       ! Calculate RothC model (1) or not (2)
namelist /spm_namelist/ cons_strsn       ! Consider nitrogen stress of SWAT type (1) or not (0)
namelist /spm_namelist/ co2_dr_vcmax25
namelist /spm_namelist/ co2_dr_jmax25
namelist /spm_namelist/ co2_dr_sla
namelist /spm_namelist/ co2_dr_rd25_fac
namelist /spm_namelist/ q_base
namelist /spm_namelist/ adjust_np_stress
namelist /spm_namelist/ adjust_e
namelist /spm_namelist/ fgdd_correct !2013/06/15i

end module spm_param
