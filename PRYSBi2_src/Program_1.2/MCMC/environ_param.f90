module environ_param

implicit none

!=====================================================
!================== For spm_crops ====================

!----- Variables previously readed -----

!::: for climate :::
real(8), allocatable :: tm(:)
real(8), allocatable :: tx(:)
real(8), allocatable :: tn(:)
real(8), allocatable :: sr(:)
real(8), allocatable :: rh(:)
real(8), allocatable :: ws(:)
real(8), allocatable :: ca(:)
real(8) :: oa
real(8) :: press

!::: for topology :::
real(8) :: lati
real(8) :: uzz

!::: for calender :::
integer, allocatable :: doy(:)
integer, allocatable :: year(:)

!::: for farming method :::
integer, allocatable :: plnt_day(:)
integer, allocatable :: hvst_day(:)
real(8), allocatable :: finp(:)

!::: for soil :::
real(8), allocatable :: sol_z(:)
real(8), allocatable :: sol_tmp(:)
real(8), allocatable :: sol_st(:)
real(8), allocatable :: sol_fc(:)
real(8), allocatable :: sol_wpmm(:)
real(8), allocatable :: sol_bd(:)
real(8), allocatable :: conv_wt(:)
real(8), allocatable :: sol_ul(:)
real(8), allocatable :: sol_clay(:)
real(8), allocatable :: soil_resid_l(:)
real(8), allocatable :: sol_aorgn(:)
real(8), allocatable :: sol_cbn(:)
real(8), allocatable :: sol_fon(:)
real(8), allocatable :: sol_fop(:)
real(8), allocatable :: sol_no3(:)
real(8), allocatable :: sol_orgn(:)
real(8), allocatable :: sol_orgp(:)
real(8), allocatable :: sol_rsd(:)
real(8), allocatable :: sol_nh3(:)
real(8), allocatable :: sol_actp(:)
real(8), allocatable :: sol_solp(:)
real(8), allocatable :: sol_stap(:)
real(8) :: soilc(5)

!----- State variables -----
integer :: itime
real(8) :: tn_next
integer :: crop_flag
integer :: non_dev
real(8) :: gdd
real(8) :: biomass
real(8) :: leaf_bio
real(8) :: cold_tm
integer :: cold_days
real(8) :: froot
real(8) :: tm_weighting(366)
real(8) :: tm_preweight(366)
integer :: dae
integer :: lg_days
real(8) :: plantn
real(8) :: plantp
real(8) :: fgdd
real(8) :: lai
real(8) :: cht
real(8) :: root_depth
real(8), allocatable :: root_bio_l(:)
real(8) :: ag_bio
real(8) :: cold_strs
real(8) :: yield
real(8) :: strsn
real(8) :: strsp
real(8) :: strsm
integer :: moist_days
real(8) :: totC
real(8) :: co2_soil
real(8) :: ga
real(8) :: npp
real(8) :: hr
real(8) :: nep
real(8) :: wdntl
real(8) :: hmn_tot
real(8) :: hmp_tot
real(8) :: rmn_tot
real(8) :: rmp_tot

!----- State variables transfered by SWAT -----
real(8) :: sepbtm
real(8) :: prec
real(8) :: surfq
real(8), allocatable :: sol_prk(:)


!==================================================
!================== For swatwt ====================

!----- Variobles previously readed -----

!::: for climate :::
real(8), allocatable :: pr(:)
real(8) :: ta
real(8) :: sx(1:366)

!::: for topology :::
real(8) :: elev
real(8) :: slp

!::: for soil :::
real(8), allocatable :: sol_alb(:)
real(8), allocatable :: sol_shc(:)
real(8) :: sol_sumfc
real(8) :: sol_sumul
real(8) :: sol_avbd

!----- State variables -----
real(8) :: snow
real(8) :: snotmp
real(8) :: canstor
real(8) :: ep_max
real(8) :: pet_day
real(8) :: sol_sw
real(8) :: es_day
real(8) :: ep_day
real(8) :: sno3up
real(8) :: canev
real(8) :: snoev
real(8) :: et_day
real(8) :: strsw
real(8) :: albd
real(8) :: snofall
real(8) :: snomlt
real(8) :: strs_fac
real(8) :: strsw_lim
real(8) :: irr_capacity


!====================================================
!=============== For observation ====================
real(8), allocatable :: obs_yield(:)


!====================================================
!=========== For soil condition calculation =========
real(8), allocatable :: sol_awc(:)
real(8), allocatable :: sol_wp(:)
real(8), allocatable :: sol_up(:)
real(8), allocatable :: sol_por(:)

!=====================================================
!================== For parameter ====================
real(8), allocatable :: params(:,:)
real(8), allocatable :: fix_params(:)
real(8), allocatable :: gdp(:)



end module environ_param
