module Mcalc_llk

	use environ_param
	use var_read_all
	use var_divide
	use spm_param
	use spm_param_fix
	use spm_crops
	use swatwt
	use control_param
	use Mdream_param

	implicit none
	private
	public :: calc_llk

	contains

	!=========================
	!=== Parameter setting ===
	!=========================
	subroutine parameter_setting(param_x)

		!---------------------------------------------------------------------
		! In this subroutine, the parameter values that was read for each grid
		! are assigned.
		!---------------------------------------------------------------------
	real(8), intent(in) :: param_x(:)

	! yield_error     = abs( param_x(1) )
	! temp_intercept  = abs( param_x(2) )
	! temp_gdpeffect  = abs( param_x(3) )
	! irr_capacity    = param_x(4)
	! cold_heat_coef1 = param_x(5)

		yield_error          = abs( param_x(1) )
		temp_adjust_strs_all = abs( param_x(2) )
		temp_ic_strs_all     = param_x(3)
		irr_capacity         = param_x(4)

	end subroutine parameter_setting

	!=====================
	!=== Chain setting ===
	!=====================
	subroutine chain_setting

		!------------------------------------------------
		! If you want change the setting for each chain,
		! please change the setting in this subroutine.
		! Following is a example.
		!------------------------------------------------

		! Example
		! chain_tm = int( dble((calc_i_chain - 1)) / 9.0d0 )
		! chain_pr = int( mod( calc_i_chain, 9 ) )
		!
		! irr_capacity = 0d0
		! if(chain_tm == 0) add_tm = -1d0 !T-1
		! if(chain_tm == 1) add_tm =  0d0 !T0
		! if(chain_tm == 2) add_tm =  1d0 !T1
		! if(chain_tm == 3) add_tm =  2d0 !T2
		! if(chain_tm == 4) add_tm =  3d0 !T3
		! if(chain_tm == 5) add_tm =  4d0 !T4
		! if(chain_tm == 6) add_tm =  6d0 !T6
		!
		! if(chain_pr == 1) add_pr = 0.5d0 !W-50
		! if(chain_pr == 2) add_pr = 0.7d0 !W-30
		! if(chain_pr == 3) add_pr = 0.8d0 !W-20
		! if(chain_pr == 4) add_pr = 0.9d0 !W-10
		! if(chain_pr == 5) add_pr = 1.0d0 !W0
		! if(chain_pr == 6) add_pr = 1.1d0 !W10
		! if(chain_pr == 7) add_pr = 1.2d0 !W20
		! if(chain_pr == 8) add_pr = 1.3d0 !W30
		! if(chain_pr == 0) then           !Winf
		! add_pr       = 1.0d0
		! irr_capacity = 1000d0
		! endif
		!
		! ca(:) = dble(ctl_ca)
		! if(ctl_adapt == 1) tmbs = tmbs + add_tm

	end subroutine chain_setting

	!==========================================
	!===== Adjust technological parameter =====
	!==========================================
	subroutine adjust_tech

	!--- Adjust stress factor ---
	if( dream_strs_fac_type == 1 ) then
		temp_year = year(itime)
		strs_fac = temp_adjust_strs_all + temp_ic_strs_all * dble(temp_year - 2006)
		if(strs_fac > 1.0d0) strs_fac = 1.0d0
		if(strs_fac < 0.0d0) strs_fac = 0.0d0
	endif

	if( dream_strs_fac_type == 2 ) then
		!--- Adjust stress factor ---
		temp_year = year(itime)
		strs_fac = temp_adjust_strs_all + temp_yyy(temp_year)
		if(strs_fac > 1.0d0)  strs_fac = 1.0d0
		if(strs_fac < 0.0d0)  strs_fac = 0.0d0
	endif

	if( dream_strs_fac_type == 3 ) then
		!--- Adjust stress factor ---
		strs_fac = temp_adjust_strs_all
		if(strs_fac > 1.0d0)  strs_fac = 1.0d0
		if(strs_fac < 0.0d0)  strs_fac = 0.0d0
	endif

	if( dream_strs_fac_type == 4 ) then
		!--- Adjust stress factor ---
		temp_year = year(itime)
		strs_fac = temp_intercept + temp_gdpeffect * temp_gdplog(temp_year)
		if(strs_fac > 1.0d0)  strs_fac = 1.0d0
		if(strs_fac < 0.0d0)  strs_fac = 0.0d0
	endif

	end subroutine adjust_tech

	!====================================
	!===== Adjust irrigation effect =====
	!====================================
	subroutine adjust_irrigation

	!--- Irrigation ---
	if(strsw < strsw_lim .and. crop_flag == 1) then
		temp_sol_st_save(:) = temp_sol_st(:)
		temp_irr = max(0d0, irr_capacity)
		do temp_isol = 1, temp_nsol
			temp_xxx = min(temp_irr, sol_fc(temp_isol) - temp_sol_st(temp_isol))
			temp_sol_st(temp_isol) = temp_sol_st(temp_isol) + temp_xxx
			temp_irr = temp_irr - temp_xxx
		enddo
		day_irrigation = max(0d0, sum(temp_sol_st(:) - temp_sol_st_save(:)))
	else
		day_irrigation = 0d0
	endif

	end subroutine adjust_irrigation

	!=======================
	!===== Calc output =====
	!=======================
	subroutine calc_output(calc_llk_est)

		!----------------------------------------------------------------------------------
		! In this subroutine, the state variables are assigned to the variables for output.
		! You must change following codes according to your purpose of the simulation.
		!----------------------------------------------------------------------------------
	real(8), intent(inout) :: calc_llk_est(iyrstt:,:)

	if( yield >= 0.0d0 ) then

		base_tm_check = base_tm_check + 1
		if( base_tm_check < 5 ) then
			if( base_tm_check > 1 ) three_tm_yr_mean(base_tm_check - 1) = tm_yr_mean / 365.00d0
		endif
		base_tm_yr_mean = sum( three_tm_yr_mean ) / 3.0d0

		pre_tm_yr_mean = tm_yr_mean / 365.0d0

		! When yield is zero
		if( yield == 0.0d0 ) then
			temp_ag_bio = 0.0d0
			temp_acc_et_day = 0.0d0
		endif

		if(temp_dim >= 1) then
			calc_llk_est(year(itime), 1) = yield ! Crop yield
		endif
		if(temp_dim >= 2) then
			calc_llk_est(year(itime), 2) = temp_ag_bio / 1000d0 ! above ground biomass (t/ha)
		endif
		if(temp_dim >= 3) then
			calc_llk_est(year(itime), 3) = temp_pd(year(itime)) ! Planting day (DOY)
		endif
		if(temp_dim >= 4) then
			calc_llk_est(year(itime), 4) = doy(itime) ! Harvest day (DOY)
		endif
		if(temp_dim >= 5) then
			calc_llk_est(year(itime), 5) = sum_irrigation ! Irrigated water (mm)
		endif
		if(temp_dim >= 6) then
			calc_llk_est(year(itime), 6) = temp_acc_et_day ! Evapotranspiration (mm)
		endif

		if(plnt_day(iyrstt) < 0) then
			calc_llk_est(:,:) = -9999.0
		endif

		! Zero clear
		temp_laimax     = 0.0d0
		temp_biomass    = 0.0d0
		temp_acc_et_day = 0.0d0
		temp_acc_npp    = 0.0d0
		temp_mean_strsw = 0.0d0
		temp_acc_strsw  = 0.0d0
		temp_min_strsw  = 1.0d0
		temp_dae        = 0
		sum_irrigation  = 0.0d0
		temp_ag_bio     = 0.0d0
		anth_flag       = 0
		tm_yr_mean      = 0.0d0

			! Calculation of base temperature of the next years
			! This calculation is needed only when ctl_adapt = 1, in which
			! The developing days of crop is adjusted even under changed climate.
			! This setting is used only when you want to know the effect of adaptation
			! on crop yield under future climate change.
			! Normal setting is ctl_adapt == 0.
		if(ctl_adapt == 0) then
			temp_xxx = base_tmbs
		endif
		if(ctl_adapt == 1 .and. base_tm_check > 4 ) then ! Use modified tmbs for remaining 5 years
			temp_xxx = base_tmbs + pre_tm_yr_mean - base_tm_yr_mean
		endif
		if(ctl_adapt == 1 .and. base_tm_check < 5 ) then ! Use standard tmbs for the first 4 years
			temp_xxx = base_tmbs
		endif
		tmbs = temp_xxx

	endif

		! Save anthesis day
		! In this model, the anthesis day is not important for crop development.
		! Therefore, we simply assume that anthesis day is the day of fgdd = 0.5.
	if ( fgdd > 0.5d0 .and. anth_flag == 0 ) then
		if(temp_dim >= 7) calc_llk_est(year(itime), 7) = doy(itime)
		anth_flag = 1
	endif

	! Calculation of variables relating to output variable
	temp_laimax  = max(temp_laimax, lai)
	temp_biomass = biomass
	temp_ag_bio  = ag_bio
	if( crop_flag == 1 ) then
		temp_acc_et_day = temp_acc_et_day + et_day
		temp_acc_npp    = temp_acc_npp + npp
		temp_acc_strsw  = temp_acc_strsw + strsw
		temp_mean_strsw = temp_acc_strsw / dble(temp_dae)
		temp_min_strsw  = min( temp_min_strsw, strsw )
		temp_dae        = temp_dae + 1
		sum_irrigation  = sum_irrigation + day_irrigation
	endif

	tm_yr_mean = tm_yr_mean + temp_tm

	end subroutine calc_output

	!===========================
	!===== Calc likelihood =====
	!===========================
	subroutine calc_likelihood(calc_llk_x, calc_llk_sdprior, calc_llk_avprior, calc_llk_est, calc_llk_llk, calc_llk_typeprior)

		real(8), intent(in)     :: calc_llk_x(:)
		integer, intent(in)     :: calc_llk_typeprior(:)
		real(8), intent(in)     :: calc_llk_avprior(:)
		real(8), intent(in)     :: calc_llk_sdprior(:)
		real(8), intent(out)    :: calc_llk_llk
		real(8), intent(in)     :: calc_llk_est(iyrstt:,:)

		if( sim_flag /= 1 ) then

			!--- Calculation of likelihood relating to prior distribution ---
			temp_llk_prior = 0.0d0
			do i_llk = 1, temp_param_dim

				if( calc_llk_typeprior(i_llk) == 1) then
					temp_var   = calc_llk_sdprior(i_llk)**2
					temp_error = calc_llk_x(i_llk) - calc_llk_avprior(i_llk)

					temp_llk_prior = temp_llk_prior - log( 2.0d0 * temp_pi * temp_var ) / 2.0d0 - ( temp_error**2 / ( 2.0d0 * temp_var ) )
				endif

			enddo

			!--- Calculation of likelihood relating to estimated variable ---
			temp_llk_est = 0.0d0
		do j_llk = iyrstt, iyrend

			if( temp_obs(j_llk, 1) == -9999d0 ) cycle ! Skip when observation data is NA.

			temp_var = yield_error

			temp_error = temp_obs(j_llk, 1) - calc_llk_est(j_llk, 1)

			temp_llk_est = temp_llk_est - log( 2.0d0 * temp_pi * temp_var ) / 2.0d0 - ( temp_error**2 / ( 2.0d0 * temp_var ) )

		enddo

		!--- Sum of log likelihood ---
			calc_llk_llk = temp_llk_prior + temp_llk_est

		endif

	if( sim_flag == 1) then
		calc_llk_llk = -9999d0
	endif

	end subroutine calc_likelihood


	!=======================
	!===== Calculation =====
	!=======================
	!-----------------------------------------------------
	! This subroutine is the main subroutine for caclating
	! crop development and likelihood using the crop model stored in the directory "PRYSBI2" and
	! the subroutines defined above.
	subroutine calc_llk(calc_llk_x, &
					& calc_llk_typeprior, &
					& calc_llk_avprior, &
					& calc_llk_sdprior, &
					& calc_llk_llk, &
					& calc_i_chain, &
					& calc_llk_est)

		implicit none

		!--- Variable declaration ---
		real(8), intent(in)     :: calc_llk_x(:)
		integer, intent(in)     :: calc_llk_typeprior(:)
		real(8), intent(inout)  :: calc_llk_avprior(:)
		real(8), intent(inout)  :: calc_llk_sdprior(:)
		real(8), intent(out)    :: calc_llk_llk
		integer, intent(in)     :: calc_i_chain
		real(8), intent(out)    :: calc_llk_est(iyrstt:,:)

		!--- Zeroclear1 ---
		add_tm = 0.0d0
		add_pr = 1.0d0
		base_tm_yr_mean       = 0.0d0
		pre_tm_yr_mean        = 0.0d0
		tm_yr_mean            = 0.0d0
		base_tm_check         = 0
		three_tm_yr_mean(1:3) = 0

		!--- Soil layer ---
		temp_nsol = size(sol_st)

		!--- Allocation ---
		allocate( temp_sol_st   (temp_nsol) )
		allocate( temp_sol_no3  (temp_nsol) )
		allocate( temp_sol_aorgn(temp_nsol) )
		allocate( temp_sol_cbn  (temp_nsol) )
		allocate( temp_sol_fon  (temp_nsol) )
		allocate( temp_sol_fop  (temp_nsol) )
		allocate( temp_sol_orgn (temp_nsol) )
		allocate( temp_sol_orgp (temp_nsol) )
		allocate( temp_sol_rsd  (temp_nsol) )
		allocate( temp_sol_nh3  (temp_nsol) )
		allocate( temp_sol_actp (temp_nsol) )
		allocate( temp_sol_solp (temp_nsol) )
		allocate( temp_sol_stap (temp_nsol) )
		allocate( temp_sol_st_save(temp_nsol))
		allocate( temp_pd (iyrstt:iyrend))
		allocate( temp_yyy (iyrstt:iyrend))
		allocate( temp_gdplog(iyrstt:iyrend))

		calc_llk_est(:,:) = -9999d0

		temp_dim        = size( calc_llk_est(iyrstt,:) )
		temp_param_dim  = size( calc_llk_x )

		sim_flag = 1
		if( dream_simulate_version == 2 ) then
			sim_flag = 0
		endif

		!--- Assign and observation data to the variables used in this subroutine ---
		allocate( temp_obs(iyrstt:iyrend, temp_dim) )
		temp_obs(:,:) = -9999.0d0

		if( sim_flag/=1 ) then
			do iyr = iyrstt, iyrend
				if(iyrstt <= iyr .and. iyr <= iyrend) then
					temp_obs(iyr, 1) = obs_yield(iyr)
				endif
			enddo
			if( iyrstt <= 2000 .and. 2000 <= iyrend .and. temp_dim >= 2 ) then
				temp_obs(iyrstt:iyrend, 2) = hvst_day(2000) ! When using the data of Sacks for calibration
			endif
			temp_obs(iyrstt, 1) = -9999.0d0
		endif

		!--- Zero clear2 ---
		temp_adjust_strs_all = 1.0d0
		temp_ic_strs_all     = 0.0d0
		temp_laimax          = 0.0d0
		temp_biomass         = 0.0d0
		temp_acc_et_day      = 0.0d0
		temp_acc_npp         = 0.0d0
		temp_mean_strsw      = 0.0d0
		temp_acc_strsw       = 0.0d0
		temp_min_strsw       = 1.0d0
		temp_dae             = 0
		temp_ag_bio          = 0.0d0
		anth_flag            = 0
		yield                = -9999d0

		!--- CO2 ---
		if( ctl_ca > 0 ) ca(:) = dble(ctl_ca)

		!--- Base temperature ---
		base_tmbs = tmbs

		!--- Assign parameter values when excuting MCMC ---
		if( sim_flag /= 1 ) then

			call parameter_setting( calc_llk_x( : ) )

		endif

		!--- Assign parameter values when excuting simulation ---
		if( sim_flag == 1 ) then

			call s_spm_param_fix
			call parameter_setting( params(i_step, :) )
			call chain_setting

		endif

		!--- Calculation relating to GDP ---
		temp_gdplog(:) = log(gdp(:))
		temp_yyy(:) = temp_intercept + temp_gdpeffect * temp_gdplog(:)
		temp_yyy(:) = temp_yyy(:) - temp_yyy(iyrstt) ! increment by GDP increase
		if(gdp(iyrstt)<0) temp_yyy(:) = 0d0 ! If there is no gdp data

		!--- Assignment of fixed parameter ---
		tgdd = fix_params(1)

		!--- Set the variables relating to slop ---
		slp       = 0.00d0

		!--- Set the variables relating to irrigation ---
		! This set up is important for irrigation
		! You may need to change according to irrigation setting.
		strsw_lim = 0.99d0

		!--- Adjust planting day ---
		temp_pd(:) = plnt_day(:) + ctl_adjust_plnt

		!--- Check plantin day ---
		if( plnt_day(iyrstt) < hvst_day(iyrstt) ) then
			hvst_lt_plnt = 1
		else
			hvst_lt_plnt = 0
		endif

		!--- Assign state variables ---
		temp_sol_st        = sol_st
		temp_sol_no3       = sol_no3
		temp_sol_aorgn     = sol_aorgn
		temp_sol_cbn       = sol_cbn
		temp_sol_fon       = sol_fon
		temp_sol_fop       = sol_fop
		temp_sol_orgn      = sol_orgn
		temp_sol_orgp      = sol_orgp
		temp_sol_rsd       = sol_rsd
		temp_sol_nh3       = sol_nh3
		temp_sol_actp      = sol_actp
		temp_sol_solp      = sol_solp
		temp_sol_stap      = sol_stap

		!--- Zero clear ---
		snow          = 0.0d0
		snotmp        = 0.0d0
		canstor       = 0.0d0
		crop_flag     = 0
		non_dev       = 0
		gdd           = 0.0d0
		biomass       = 0.0d0
		soil_resid_l  = 0.0d0
		cold_tm       = 0.0d0
		cold_days     = 0
		froot         = 0.0d0
		tm_weighting  = 0.0d0
		tm_preweight  = 0.0d0
		dae           = 0
		lg_days       = 0
		plantn        = 0.0d0
		plantp        = 0.0d0
		fgdd          = 0.0d0
		lai           = 0.0d0
		cht           = 0.0d0
		root_depth    = 0.0d0
		root_bio_l    = 0.0d0
		ag_bio        = 0.0d0
		sum_irrigation= 0.0d0

		! Spin up
		if(ctl_nspin /= 0) then
			do ispin = 1, ctl_nspin
				do itime = 1, 365

						!--- Adjust temperature and precipitation ---
						! This code is effective when changing temperature and Precipitation
						! for each chain. For normal simulation, ignore this code.
						temp_tm = tm(itime) + add_tm
						temp_tx = tx(itime) + add_tm
						temp_tn = tn(itime) + add_tm
						temp_pr = pr(itime) * add_pr
						temp_ta = ta + add_tm

						!--- Soil water ---
						call s_swatwt( &
						doy(itime)       , & !in    Day of Year
						temp_tm          , & !in    Average temperature of the day (deg C)
						temp_tx          , & !in    Maximum temperature of the day (deg C)
						temp_tn          , & !in    Minimum temperature of the day (deg C)
						temp_ta          , & !in    Average temperature over the simulation time (deg C)
						temp_pr          , & !in    Precipitation of the day (mm)
						sr(itime)        , & !in    Solar radiation of the day (MJ/m^2)
						sx(doy(itime))   , & !in    Maximum possible radiation of the day (MJ/m^2)
						rh(itime)        , & !in    Relative humidity (%)
						ws(itime)        , & !in    Wind speed (m/s)
						ca(year(itime))  , & !in    Atmospheric CO2 concentration (ppm)
						elev             , & !in    Elevation (m)
						slp              , & !in    Fraction of the slop of the ground (%)
						sol_alb          , & !in    Albed of each soil layer
						sol_fc           , & !in    Field capacity of each soil layer (mm)
						sol_ul           , & !in    Water content of each layer at satulation (mm)
						sol_shc          , & !in    Saturated hydraulic conductivity (mm/hr)
						sol_z            , & !in    Length from the top of the soil to the botom of the each soil layer (mm)
						sol_sumfc        , & !in    Field capacity of the entire soil (mm)
						sol_sumul        , & !in    Water content of the entir soil at satulation (mm)
						sol_avbd         , & !in    Average albed of the soil
						lai              , & !in    LAI
						fgdd             , & !in    Fraction of growing degree days to GDD needed for maturity
						cht              , & !in    Canopy height (m)
						biomass          , & !in    Biomass of the plant (kg/ha)
						root_depth       , & !in    Current rooting depth (mm)
						snow             , & !inout Amount of snow (mm)
						snotmp           , & !inout Snow temperature (deg C)
						canstor          , & !inout Water contained in canopy (mm)
						temp_sol_st      , & !inout Amount of water in each soil layer at the day (mm)
						temp_sol_no3     , & !inout Amonut of nitrogen strored in the nitrate pool in each soil layer (kg/ha)
						prec             , & !out   Precipitation depth (at the ground level) (mm)
						surfq            , & !out   Accumulated runoff or rainfall excess (mm)
						snofall          , & !out   Amount of snow falling (mm)
						snomlt           , & !out   Amount of snow melting (mm)
						albd             , & !out   Albedo
						ep_max           , & !out   Potential ET of plant (mm) (considering LAI)
						pet_day          , & !out   Potential ET (mm) (considering standerd 40cm alphalpha)
						sol_sw           , & !out   Amount of water stored in the entire soil profile (mm)
						es_day           , & !out   Actual amount of evaporation from soil (mm)
						ep_day           , & !out   Actual amount of transpiration (mm)
						sno3up           , & !out   Amount of nitrate moving upward in the soil (kg/ha)
						canev            , & !out   Amount of water evaporated from canopy (mm)
						snoev            , & !out   Amount of water in snow lost through sublimation (mm)
						sepbtm           , & !out   Percolation from bottom of entire soil profile (mm)
						sol_prk          , & !out   Percolation from each soil layer (mm)
						sol_tmp          , & !out   Soil temperature of each layer (deg C)
						et_day           , & !out   Actual amount of evapotranspiration (mm)
						strsw            )   !out   Water stress (0: heigh stress, 1: low stress)

						!--- Adjust water stress ---
						if( ctl_cut_strsw == 1 ) strsw = 1.0d0

						!--- Adjust stress factor
						call adjust_tech
						call adjust_irrigation

						!--- Set the temperature of previous day
						if(itime < ntime) tn_next = tn(itime+1)
						temp_tn_next = tn_next + add_tm

						call s_spm_crops( &
						strs_fac,              & !in    Stress factor                                                  (-)
						itime,                 & !in    Time                                                           (-)
						temp_pd(year(itime)),  & !in    Planting day                                                   (day)
						temp_tm,               & !in    Average temperature of the day                                 (deg C)
						sol_z,                 & !in    Soil depth of below boundary of each layer                     (mm)
						sol_tmp,               & !in    Soil temperature of each layer                                 (deg C)
						doy(itime),            & !in    Day of years                                                   (day)
						lati,                  & !in    Latitude                                                       (deg)
						sr(itime),             & !in    Solar radiation                                                (MJ/m2/day)
						temp_tx,               & !in    Maximum temperature of the day                                 (deg C)
						temp_tn,               & !in    Minimum temperature of the day                                 (deg C)
						temp_tn_next,          & !in    Minimum temperature of the next day                            (deg C)
						rh(itime),             & !in    Relative humidity                                              (%)
						ca(year(itime)),       & !in    Atomospheric concentration of CO2                              (ppm)
						oa,                    & !in    Atomospheric concentration of O2                               (ppm)
						strsw,                 & !in    Stress factor transfered from the other program                (-)
						ws(itime),             & !in    Wind speed at uzz                                              (m/s)
						uzz,                   & !in    Height at which wind speed measured                            (m)
						year(itime),           & !in    Year                                                           (year)
						press,                 & !in    Atomosphere pressure                                           (Pa)
						sepbtm,                & !in    Percolation from bottom of entire soil profile                 (mm)
						prec,                  & !in    Precipitation depth (at the ground level)                      (mm)
						surfq,                 & !in    Accumulated runoff or rainfall excess                          (mm)
						sol_st,                & !in    Amount of water in each soil layer at the day                  (mm)
						sol_fc,                & !in    Field capacity of each soil layer                              (mm)
						sol_wpmm,              & !in    Wilting point of each soil layer                               (mm)
						sol_bd,                & !in    Bulk density of each soil layer                                (Mg/m^3)
						conv_wt,               & !in    Factor which converts kg/kg soil to kg/ha                      (kg/ha)
						sol_prk,               & !in    Percolation from each soil layer                               (mm)
						sol_ul,                & !in    Water content of each layer at satulation                      (mm)
						sol_clay,              & !in    Percent clay content in soil material                          (%)
						finp(itime),           & !in    Fertilizer C input                                             (t/ha)
						crop_flag,             & !inout There is crop (1) or not (0)                                   (-)
						non_dev,               & !inout The number of non-developing days                              (day)
						gdd,                   & !inout Groweing degree days                                           (deg C)
						biomass,               & !inout Total biomass                                                  (kg dry/ha)
						leaf_bio,              & !inout Leaf biomass                                                   (kg dry/ha)
						soil_resid_l,          & !inout Residuals in each layer soil                                   (kg dry/ha)
						cold_tm,               & !inout Average temperature during cold stress period                  (deg C)
						cold_days,             & !inout The number of days counted during cold stress perriod          (day)
						froot,                 & !inout Root ratio                                                     (-)
						tm_weighting,          & !inout Saving variables for cold stress type 1                        (-)
						tm_preweight,          & !inout Saving variables for cold stress type 1                        (-)
						dae,                   & !inout Day after emergence (initial = 0)                              (days)
						lg_days,               & !inout Leaf growth days                                               (days)
						temp_sol_aorgn,        & !inout Amount of nitrogen stored in humus in each layer               (kg/ha)
						temp_sol_cbn,          & !inout Percent organic carbon in each soil layer                      (%)
						temp_sol_fon,          & !inout Amount of organic N stored in the fresh        in each layer   (kg/ha)
						temp_sol_fop,          & !inout Amount of organic P stored in the fresh        in each layer   (kg/ha)
						temp_sol_no3,          & !inout Amount of mineral N stored in the nitrate pool in each layer   (kg/ha)
						temp_sol_orgn,         & !inout Amount of organic N stored in the stable pool  in each layer   (kg/ha)
						temp_sol_orgp,         & !inout Amount of organic P stored in the stable pool  in each layer   (kg/ha)
						temp_sol_rsd,          & !inout Amount of organic matter in each soil layer                    (kg/ha)
						temp_sol_nh3,          & !inout Amount of mineral N stored in the ammonium     in each layer   (kg/ha)
						temp_sol_actp,         & !inout Amount of organic P stored in the active pool  in each layer   (kg/ha)
						temp_sol_solp,         & !inout Amount of mineral P stored in the solution     in each layer   (kg/ha)
						temp_sol_stap,         & !inout Amount of mineral P stored in the stable pool  in each layer   (kg/ha)
						plantn,                & !inout Amount of N in plant biomass                                   (kg/ha)
						plantp,                & !inout Amount of P in plant biomass                                   (kg/ha)
						soilc,                 & !inout Amount of Soil C in each RothC compartment                     (tC/ha)
						fgdd,                  & !out   gdd / tgdd                                                     (rate)
						lai,                   & !out   LAI                                                            (ha/ha)
						cht,                   & !out   Canopy height                                                  (m)
						root_depth,            & !out   Root depth                                                     (mm)
						root_bio_l,            & !out   Root biomass of each layer                                     (kg dry/ha)
						ag_bio,                & !out   Avobe ground biomass                                           (kg dry/ha)
						cold_strs,             & !out   Cold stress                                                    (-)
						yield,                 & !out   Yield                                                          (t/ha)
						strsn,                 & !out   Nitrogen stress (0: heigh stress, 1: low stress)               (-)
						strsp,                 & !out   Phosphorus stress (0: heigh stress, 1: low stress)             (-)
						totC,                  & !out   Total C in the first soil layer                                (tC/ha)
						co2_soil,              & !out   CO2 efflux from soil                                           (tC/ha/day)
						wdntl,                 & !out   Amount of nitrogen lost by denitrification                     (kg/ha)
						hmn_tot,               & !out   Amount of nitrogen mineralized from sol_aorgn in soil profile  (kg/ha)
						hmp_tot,               & !out   Amount of phosphorus mineralized from sol_orgp in soil profile (kg/ha)
						rmn_tot,               & !out   Amount of nitrogen decomposed from sol_fon in soil profile     (kg/ha)
						rmp_tot,               & !out   Amount of phosphorus docomposed from sol_fop in soil profile   (kg/ha)
						ga,                    & !out   Aerodynamic conductance                                        (m/s)
						npp,                   & !out   Net Primary Production                                         (μmol/m2/day)
						hr,                    & !out   Heterotrophic respiration                                      (μmol/m2/day)
						nep)                     !out   Net primary Ecosystem Production                               (μmol/m2/day)

				enddo
			enddo
		endif

		! Calculate from here
		sum_irrigation = 0d0
		do itime = 1, ntime

			!--- Adjust temperature and precipitation ---
			! This code is effective when changing temperature and Precipitation
			! for each chain. For normal simulation, ignore this code.
			temp_tm = tm(itime) + add_tm
			temp_tx = tx(itime) + add_tm
			temp_tn = tn(itime) + add_tm
			temp_pr = pr(itime) * add_pr
			temp_ta = ta + add_tm

			call s_swatwt( &
			doy(itime)       , & !in    Day of Year
			temp_tm          , & !in    Average temperature of the day (deg C)
			temp_tx          , & !in    Maximum temperature of the day (deg C)
			temp_tn          , & !in    Minimum temperature of the day (deg C)
			temp_ta          , & !in    Average temperature over the simulation time (deg C)
			temp_pr          , & !in    Precipitation of the day (mm)
			sr(itime)        , & !in    Solar radiation of the day (MJ/m^2)
			sx(doy(itime))   , & !in    Maximum possible radiation of the day (MJ/m^2)
			rh(itime)        , & !in    Relative humidity (%)
			ws(itime)        , & !in    Wind speed (m/s)
			ca(year(itime))  , & !in    Atmospheric CO2 concentration (ppm)
			elev             , & !in    Elevation (m)
			slp              , & !in    Fraction of the slop of the ground (%)
			sol_alb          , & !in    Albed of each soil layer
			sol_fc           , & !in    Field capacity of each soil layer (mm)
			sol_ul           , & !in    Water content of each layer at satulation (mm)
			sol_shc          , & !in    Saturated hydraulic conductivity (mm/hr)
			sol_z            , & !in    Length from the top of the soil to the botom of the each soil layer (mm)
			sol_sumfc        , & !in    Field capacity of the entire soil (mm)
			sol_sumul        , & !in    Water content of the entir soil at satulation (mm)
			sol_avbd         , & !in    Average albed of the soil
			lai              , & !in    LAI
			fgdd             , & !in    Fraction of growing degree days to GDD needed for maturity
			cht              , & !in    Canopy height (m)
			biomass          , & !in    Biomass of the plant (kg/ha)
			root_depth       , & !in    Current rooting depth (mm)
			snow             , & !inout Amount of snow (mm)
			snotmp           , & !inout Snow temperature (deg C)
			canstor          , & !inout Water contained in canopy (mm)
			temp_sol_st      , & !inout Amount of water in each soil layer at the day (mm)
			temp_sol_no3     , & !inout Amonut of nitrogen strored in the nitrate pool in each soil layer (kg/ha)
			prec             , & !out   Precipitation depth (at the ground level) (mm)
			surfq            , & !out   Accumulated runoff or rainfall excess (mm)
			snofall          , & !out   Amount of snow falling (mm)
			snomlt           , & !out   Amount of snow melting (mm)
			albd             , & !out   Albedo
			ep_max           , & !out   Potential ET of plant (mm) (considering LAI)
			pet_day          , & !out   Potential ET (mm) (considering standerd 40cm alphalpha)
			sol_sw           , & !out   Amount of water stored in the entire soil profile (mm)
			es_day           , & !out   Actual amount of evaporation from soil (mm)
			ep_day           , & !out   Actual amount of transpiration (mm)
			sno3up           , & !out   Amount of nitrate moving upward in the soil (kg/ha)
			canev            , & !out   Amount of water evaporated from canopy (mm)
			snoev            , & !out   Amount of water in snow lost through sublimation (mm)
			sepbtm           , & !out   Percolation from bottom of entire soil profile (mm)
			sol_prk          , & !out   Percolation from each soil layer (mm)
			sol_tmp          , & !out   Soil temperature of each layer (deg C)
			et_day           , & !out   Actual amount of evapotranspiration (mm)
			strsw            )   !out   Water stress (0: heigh stress, 1: low stress)

			!--- Adjust water stress ---
			if( ctl_cut_strsw == 1 ) strsw = 1.0d0

			!--- Adjust stress factor
			call adjust_tech
			call adjust_irrigation

			!--- Set the temperature of previous day ---
			if(itime < ntime) tn_next = tn(itime+1)

			!--- Input N fertilizer ---
			if( doy(itime) == temp_pd(year(itime)) ) then
				temp_sol_nh3(1) = temp_sol_nh3(1) + 60.0 / 2.0 / 2.0
				temp_sol_no3(1) = temp_sol_no3(1) + 60.0 / 2.0 / 2.0
			endif
			if( doy(itime) == temp_pd(year(itime)) + 30 ) then
				temp_sol_nh3(1) = temp_sol_nh3(1) + 60.0 / 2.0 / 2.0
				temp_sol_no3(1) = temp_sol_no3(1) + 60.0 / 2.0 / 2.0
			endif

			temp_tn_next = tn_next + add_tm

			call s_spm_crops( &
			strs_fac,              & !in    Stress factor                                                  (-)
			itime,                 & !in    Time                                                           (-)
			temp_pd(year(itime)),  & !in    Planting day                                                   (day)
			temp_tm,               & !in    Average temperature of the day                                 (deg C)
			sol_z,                 & !in    Soil depth of below boundary of each layer                     (mm)
			sol_tmp,               & !in    Soil temperature of each layer                                 (deg C)
			doy(itime),            & !in    Day of years                                                   (day)
			lati,                  & !in    Latitude                                                       (deg)
			sr(itime),             & !in    Solar radiation                                                (MJ/m2/day)
			temp_tx,               & !in    Maximum temperature of the day                                 (deg C)
			temp_tn,               & !in    Minimum temperature of the day                                 (deg C)
			temp_tn_next,          & !in    Minimum temperature of the next day                            (deg C)
			rh(itime),             & !in    Relative humidity                                              (%)
			ca(year(itime)),       & !in    Atomospheric concentration of CO2                              (ppm)
			oa,                    & !in    Atomospheric concentration of O2                               (ppm)
			strsw,                 & !in    Stress factor transfered from the other program                (-)
			ws(itime),             & !in    Wind speed at uzz                                              (m/s)
			uzz,                   & !in    Height at which wind speed measured                            (m)
			year(itime),           & !in    Year                                                           (year)
			press,                 & !in    Atomosphere pressure                                           (Pa)
			sepbtm,                & !in    Percolation from bottom of entire soil profile                 (mm)
			prec,                  & !in    Precipitation depth (at the ground level)                      (mm)
			surfq,                 & !in    Accumulated runoff or rainfall excess                          (mm)
			sol_st,                & !in    Amount of water in each soil layer at the day                  (mm)
			sol_fc,                & !in    Field capacity of each soil layer                              (mm)
			sol_wpmm,              & !in    Wilting point of each soil layer                               (mm)
			sol_bd,                & !in    Bulk density of each soil layer                                (Mg/m^3)
			conv_wt,               & !in    Factor which converts kg/kg soil to kg/ha                      (kg/ha)
			sol_prk,               & !in    Percolation from each soil layer                               (mm)
			sol_ul,                & !in    Water content of each layer at satulation                      (mm)
			sol_clay,              & !in    Percent clay content in soil material                          (%)
			finp(itime),           & !in    Fertilizer C input                                             (t/ha)
			crop_flag,             & !inout There is crop (1) or not (0)                                   (-)
			non_dev,               & !inout The number of non-developing days                              (day)
			gdd,                   & !inout Groweing degree days                                           (deg C)
			biomass,               & !inout Total biomass                                                  (kg dry/ha)
			leaf_bio,              & !inout Leaf biomass                                                   (kg dry/ha)
			soil_resid_l,          & !inout Residuals in each layer soil                                   (kg dry/ha)
			cold_tm,               & !inout Average temperature during cold stress period                  (deg C)
			cold_days,             & !inout The number of days counted during cold stress perriod          (day)
			froot,                 & !inout Root ratio                                                     (-)
			tm_weighting,          & !inout Saving variables for cold stress type 1                        (-)
			tm_preweight,          & !inout Saving variables for cold stress type 1                        (-)
			dae,                   & !inout Day after emergence (initial = 0)                              (days)
			lg_days,               & !inout Leaf growth days                                               (days)
			temp_sol_aorgn,        & !inout Amount of nitrogen stored in humus in each layer               (kg/ha)
			temp_sol_cbn,          & !inout Percent organic carbon in each soil layer                      (%)
			temp_sol_fon,          & !inout Amount of organic N stored in the fresh        in each layer   (kg/ha)
			temp_sol_fop,          & !inout Amount of organic P stored in the fresh        in each layer   (kg/ha)
			temp_sol_no3,          & !inout Amount of mineral N stored in the nitrate pool in each layer   (kg/ha)
			temp_sol_orgn,         & !inout Amount of organic N stored in the stable pool  in each layer   (kg/ha)
			temp_sol_orgp,         & !inout Amount of organic P stored in the stable pool  in each layer   (kg/ha)
			temp_sol_rsd,          & !inout Amount of organic matter in each soil layer                    (kg/ha)
			temp_sol_nh3,          & !inout Amount of mineral N stored in the ammonium     in each layer   (kg/ha)
			temp_sol_actp,         & !inout Amount of organic P stored in the active pool  in each layer   (kg/ha)
			temp_sol_solp,         & !inout Amount of mineral P stored in the solution     in each layer   (kg/ha)
			temp_sol_stap,         & !inout Amount of mineral P stored in the stable pool  in each layer   (kg/ha)
			plantn,                & !inout Amount of N in plant biomass                                   (kg/ha)
			plantp,                & !inout Amount of P in plant biomass                                   (kg/ha)
			soilc,                 & !inout Amount of Soil C in each RothC compartment                     (tC/ha)
			fgdd,                  & !out   gdd / tgdd                                                     (rate)
			lai,                   & !out   LAI                                                            (ha/ha)
			cht,                   & !out   Canopy height                                                  (m)
			root_depth,            & !out   Root depth                                                     (mm)
			root_bio_l,            & !out   Root biomass of each layer                                     (kg dry/ha)
			ag_bio,                & !out   Avobe ground biomass                                           (kg dry/ha)
			cold_strs,             & !out   Cold stress                                                    (-)
			yield,                 & !out   Yield                                                          (t/ha)
			strsn,                 & !out   Nitrogen stress (0: heigh stress, 1: low stress)               (-)
			strsp,                 & !out   Phosphorus stress (0: heigh stress, 1: low stress)             (-)
			totC,                  & !out   Total C in the first soil layer                                (tC/ha)
			co2_soil,              & !out   CO2 efflux from soil                                           (tC/ha/day)
			wdntl,                 & !out   Amount of nitrogen lost by denitrification                     (kg/ha)
			hmn_tot,               & !out   Amount of nitrogen mineralized from sol_aorgn in soil profile  (kg/ha)
			hmp_tot,               & !out   Amount of phosphorus mineralized from sol_orgp in soil profile (kg/ha)
			rmn_tot,               & !out   Amount of nitrogen decomposed from sol_fon in soil profile     (kg/ha)
			rmp_tot,               & !out   Amount of phosphorus docomposed from sol_fop in soil profile   (kg/ha)
			ga,                    & !out   Aerodynamic conductance                                        (m/s)
			npp,                   & !out   Net Primary Production                                         (μmol/m2/day)
			hr,                    & !out   Heterotrophic respiration                                      (μmol/m2/day)
			nep)                     !out   Net primary Ecosystem Production                               (μmol/m2/day)

			call calc_output(calc_llk_est)

		enddo

		call calc_likelihood(calc_llk_x, calc_llk_sdprior, calc_llk_avprior, calc_llk_est, calc_llk_llk, calc_llk_typeprior)

		!--- deallocation ---
		deallocate( temp_sol_st )
		deallocate( temp_sol_no3 )
		deallocate( temp_sol_aorgn )
		deallocate( temp_sol_cbn )
		deallocate( temp_sol_fon )
		deallocate( temp_sol_fop )
		deallocate( temp_sol_orgn )
		deallocate( temp_sol_orgp )
		deallocate( temp_sol_rsd )
		deallocate( temp_sol_nh3 )
		deallocate( temp_sol_actp )
		deallocate( temp_sol_solp )
		deallocate( temp_sol_stap )
		deallocate( temp_pd )
		deallocate( temp_sol_st_save )
		deallocate( temp_yyy )
		deallocate( temp_gdplog )
		deallocate( temp_obs )

	end subroutine calc_llk

end module Mcalc_llk
