module Mdream_param

implicit none

integer :: dream_nstep
integer :: dream_nburn
integer :: dream_minstep
integer :: dream_param_interval
integer :: dream_est_interval
integer :: dream_gelman_interval
integer :: dream_ncr
integer :: dream_est_dim
integer :: dream_est_n
integer :: dream_nchain
integer :: dream_ndim

namelist/dream_list/dream_nstep, dream_nburn, dream_minstep, dream_param_interval, dream_est_interval, &
				& dream_gelman_interval, dream_ncr, dream_est_dim, dream_est_n, dream_nchain, dream_ndim


!----- Information about distribution -----
integer :: dream_x_typeprior( val_dream_ndim ) ! The distribution type of prior (Read) uniform=0, normal=1
real(8) :: dream_x_lim( val_dream_ndim, 2 )    ! Boundary of posterior distribution (Read)
real(8) :: dream_x_avprior( val_dream_ndim )   ! The means of prior distribution (Read)
real(8) :: dream_x_sdprior( val_dream_ndim )   ! The S.D. of prior distribution (Read)


!----- DREAM setting -----
real(8) :: dream_b     ! The width of e (Read)
real(8) :: dream_bdash ! The S.D. of epsilon (Read)
integer :: dream_delta ! The number of pairs of chains from which z is generated (Read)

character(7) :: dream_gcode_all(51200)    ! grid codes for all 52100 grids !2013/07/16i
real(8)      :: dream_tphu_av_each(51200) ! the mean of prior distribution for all grid if considering !2013/07/16i
real(8)      :: dream_tphu_sd_each(51200) ! the SD   of prior distribution for all grid if considering !2013/07/16i


!----- Variables -----
real(8) :: dream_cr( val_dream_nchain )   ! Crossing probability
integer :: dream_change( val_dream_ndim ) ! Change(1) or not(0)
real(8) :: dream_diff( val_dream_ndim )
real(8) :: dream_ddash
integer :: dream_inlimit( val_dream_ndim ) ! z is in limit(1) or not(0)


!----- Estimated variables -----
real(8) :: dream_x( val_dream_ndim, val_dream_nchain ) ! Posterior distribution
real(8) :: dream_z( val_dream_ndim, val_dream_nchain ) ! Candidate points
real(8) :: dream_x_llk( val_dream_nchain ) ! Log likelihood of x vector
real(8) :: dream_z_llk( val_dream_nchain ) ! Log likelihood of z vector
real(8), allocatable :: dream_est_x( :, :, : )
real(8), allocatable :: dream_est_z( :, :, : )
real(8) :: dream_alpha( val_dream_nchain )
real(8) :: dream_rhat( val_dream_ndim )
real(8) :: dream_ohm( val_dream_nchain )
real(8) :: dream_iqr
real(8) :: dream_q1
integer :: dream_minllk_loc
real(8) :: dream_stop( val_dream_ndim )
real(8), allocatable :: dream_pm( : )
real(8), allocatable :: dream_dm( : )


!----- Variables for save -----
real(8) :: dream_x_all( val_dream_ndim, val_dream_nchain, val_dream_nstep)
real(8) :: dream_x_llk_all( val_dream_nchain, val_dream_nstep )
real(8) :: dream_rhat_all( val_dream_ndim )
real(8), allocatable :: dream_est_x_out( :, :, : )


!----- Variables for output -----
integer :: dream_x_rec
integer :: dream_x_llk_rec
integer :: dream_alpha_rec
integer :: dream_rhat_rec
integer :: dream_est_x_rec
character(1000) :: dream_ofile
character(1000) :: dream_odir_name
character(1000) :: dream_ofile_name


!----- Temporary variables -----
real(8) :: temp_r(100)
integer :: temp_i(100)
real(8) :: temp_rr(100)
integer :: temp_si(2)
real(8) :: temp_x( val_dream_ndim )
real(8) :: temp_gelman( val_dream_nchain, val_dream_nstep )
character(10) :: bdate_time(3)
integer :: idate_time(8)

!----- Control variables -----
integer :: i_mcmc, i_chain, i_dim, i_delta, i_step


!----- Others -----
real(8), parameter :: temp_weight(1:100) = 1d0
integer :: dream_simulate_version = val_dream_simulate_version
integer :: dream_strs_fac_type = val_dream_strs_fac_type

!-------------------------
!----- For Mcalc_llk -----
!-------------------------
real(8), allocatable :: temp_sol_st(:)
real(8), allocatable :: temp_sol_no3(:)
real(8), allocatable :: temp_sol_aorgn(:)
real(8), allocatable :: temp_sol_cbn(:)
real(8), allocatable :: temp_sol_fon(:)
real(8), allocatable :: temp_sol_fop(:)
real(8), allocatable :: temp_sol_orgn(:)
real(8), allocatable :: temp_sol_orgp(:)
real(8), allocatable :: temp_sol_rsd(:)
real(8), allocatable :: temp_sol_nh3(:)
real(8), allocatable :: temp_sol_actp(:)
real(8), allocatable :: temp_sol_solp(:)
real(8), allocatable :: temp_sol_stap(:)
integer, allocatable :: temp_pd(:)
real(8), allocatable :: temp_obs(:,:)
real(8), allocatable :: temp_sol_st_save(:)
real(8), allocatable :: temp_yyy(:)
real(8), allocatable :: temp_gdplog(:)
real(8) :: temp_llk_prior
real(8) :: temp_llk_est
real(8) :: temp_var
real(8) :: temp_error
real(8) :: temp_random
real(8) :: temp_adjust_strs_all
real(8) :: temp_ic_strs_all
real(8) :: temp_adj
real(8) :: temp_laimax
real(8) :: temp_biomass
real(8) :: temp_acc_et_day
real(8) :: temp_acc_npp
real(8) :: temp_mean_strsw
real(8) :: temp_acc_strsw
real(8) :: temp_min_strsw
real(8) :: temp_irr, temp_xxx, temp_intercept, temp_gdpeffect
real(8) :: temp_ag_bio
integer :: temp_ntime, temp_dim, i_llk, j_llk, temp_param_dim, iyr, ispin
integer :: temp_nsol, temp_isol
integer :: temp_year
integer :: temp_dae
integer :: anth_flag

real(8), parameter :: temp_pi = asin(1d0)*2d0

real(8) :: obs_av, est_av, cov_obs_est, obs_dif, est_dif, cor_coef
real(8) :: sum_irrigation, day_irrigation
integer :: av_n

integer :: chain_tm, chain_pr
integer :: chain_irr, chain_plnt
integer :: base_tm_check

real(8) :: add_tm, add_pr
real(8) :: temp_ta, temp_tn, temp_tx, temp_pr, temp_tm, temp_tn_next
real(8) :: base_tm_yr_mean, pre_tm_yr_mean, tm_yr_mean, three_tm_yr_mean(3)

real(8) :: base_tmbs, yield_error

end module Mdream_param
