module spm_crops

use calc_biomass
use cold_stress
use micro_met
use development
use harvest
use maintenance
use photosynthesis
use planting
use root_structure
use spm_param
use shoot_structure
use param_regulation
use swatnut
use swatnut_param
use day_rothc
use day_rothc_param

implicit none

private
public :: s_spm_crops

contains

subroutine s_spm_crops( &
  strs_fac,         & !in    Stress factor                                                  (-)
	itime,            & !in    Time                                                           (-)
	plnt_dayin,       & !in    Planting day                                                   (day)
	tm,               & !in    Average temperature of the day                                 (deg C)
	sol_z,            & !in    Soil depth of below boundary of each layer                     (mm)
	sol_tmp,          & !in    Soil temperature of each layer                                 (deg C)
	doy,              & !in    Day of years                                                   (day)
	lati,             & !in    Latitude                                                       (deg)
	sr,               & !in    Solar radiation                                                (MJ/m2/day)
	tx,               & !in    Maximum temperature of the day                                 (deg C)
	tn,               & !in    Minimum temperature of the day                                 (deg C)
	tn_next,          & !in    Minimum temperature of the next day                            (deg C)
	rh,               & !in    Relative humidity                                              (%)
	ca,               & !in    Atomospheric concentration of CO2                              (ppm)
	oa,               & !in    Atomospheric concentration of O2                               (ppm)
	strsw,            & !in    Stress factor transfered from the other program                (-)
	ws,               & !in    Wind speed at uzz                                              (m/s)
	uzz,              & !in    Height at which wind speed measured                            (m)
	year,             & !in    Year                                                           (year)
	press,            & !in    Atomosphere pressure                                           (Pa)
	sepbtm,           & !in    Percolation from bottom of entire soil profile                 (mm)
	prec,             & !in    Precipitation depth (at the ground level)                      (mm)
	surfq,            & !in    Accumulated runoff or rainfall excess                          (mm)
	sol_st,           & !in    Amount of water in each soil layer at the day                  (mm)
	sol_fc,           & !in    Field capacity of each soil layer                              (mm)
	sol_wpmm,         & !in    Wilting point of each soil layer                               (mm)
	sol_bd,           & !in    Bulk density of each soil layer                                (Mg/m^3)
	conv_wt,          & !in    Factor which converts kg/kg soil to kg/ha                      (kg/ha)
	sol_prk,          & !in    Percolation from each soil layer                               (mm)
	sol_ul,           & !in    Water content of each layer at satulation                      (mm)
	sol_clay,         & !in    Percent clay content in soil material                          (%)
	finp,             & !in    Fertilizer C input                                             (t/ha)
	crop_flag,        & !inout There is crop (1) or not (0)                                   (-)
	non_dev,          & !inout The number of non-developing days                              (day)
	gdd,              & !inout Groweing degree days                                           (deg C)
	biomass,          & !inout Total biomass                                                  (kg dry/ha)
	leaf_bio,         & !inout Leaf biomass                                                   (kg dry/ha)
	soil_resid_l,     & !inout Residuals in each layer soil                                   (kg dry/ha)
	cold_tm,          & !inout Average temperature during cold stress period                  (deg C)
	cold_days,        & !inout The number of days counted during cold stress perriod          (day)
	froot,            & !inout Root ratio                                                     (-)
	tm_weighting,     & !inout Saving variables for cold stress type 1                        (-)
	tm_preweight,     & !inout Saving variables for cold stress type 1                        (-)
	dae,              & !inout Day after emergence (initial = 0)                              (days)
	lg_days,          & !inout Leaf growth days                                               (days)
	sol_aorgn,        & !inout Amount of nitrogen stored in humus in each layer               (kg/ha)
	sol_cbn,          & !inout Percent organic carbon in each soil layer                      (%)
	sol_fon,          & !inout Amount of organic N stored in the fresh        in each layer   (kg/ha)
	sol_fop,          & !inout Amount of organic P stored in the fresh        in each layer   (kg/ha)
	sol_no3,          & !inout Amount of mineral N stored in the nitrate pool in each layer   (kg/ha)
	sol_orgn,         & !inout Amount of organic N stored in the stable pool  in each layer   (kg/ha)
	sol_orgp,         & !inout Amount of organic P stored in the stable pool  in each layer   (kg/ha)
	sol_rsd,          & !inout Amount of organic matter in each soil layer                    (kg/ha)
	sol_nh3,          & !inout Amount of mineral N stored in the ammonium     in each layer   (kg/ha)
	sol_actp,         & !inout Amount of organic P stored in the active pool  in each layer   (kg/ha)
	sol_solp,         & !inout Amount of mineral P stored in the solution     in each layer   (kg/ha)
	sol_stap,         & !inout Amount of mineral P stored in the stable pool  in each layer   (kg/ha)
	plantn,           & !inout Amount of N in plant biomass                                   (kg/ha)
	plantp,           & !inout Amount of P in plant biomass                                   (kg/ha)
	soilc,            & !inout Amount of Soil C in each RothC compartment                     (tC/ha)
	fgdd,             & !out   gdd / tgdd                                                     (rate)
	lai,              & !out   LAI                                                            (ha/ha)
	cht,              & !out   Canopy height                                                  (m)
	root_depth,       & !out   Root depth                                                     (mm)
	root_bio_l,       & !out   Root biomass of each layer                                     (kg dry/ha)
	ag_bio,           & !out   Avobe ground biomass                                           (kg dry/ha)
	cold_strs,        & !out   Cold stress                                                    (-)
	yield,            & !out   Yield                                                          (t/ha)
	strsn,            & !out   Nitrogen stress (0: heigh stress, 1: low stress)               (-)
	strsp,            & !out   Phosphorus stress (0: heigh stress, 1: low stress)             (-)
	totC,             & !out   Total C in the first soil layer                                (tC/ha)
	co2_soil,         & !out   CO2 efflux from soil                                           (tC/ha/day)
	wdntl,            & !out   Amount of nitrogen lost by denitrification                     (kg/ha)
	hmn_tot,          & !out   Amount of nitrogen mineralized from sol_aorgn in soil profile  (kg/ha)
	hmp_tot,          & !out   Amount of phosphorus mineralized from sol_orgp in soil profile (kg/ha)
	rmn_tot,          & !out   Amount of nitrogen decomposed from sol_fon in soil profile     (kg/ha)
	rmp_tot,          & !out   Amount of phosphorus docomposed from sol_fop in soil profile   (kg/ha)
	ga,               & !out   Aerodynamic conductance                                        (m/s)
	npp,              & !out   Net Primary Production                                         (µmol/m2/day)
	hr,               & !out   Heterotrophic respiration                                      (µmol/m2/day)
	nep)                !out   Net primary Ecosystem Production                               (µmol/m2/day)

	implicit none

	!----- declare statements of arguments -----
  real(8), intent(in)    :: strs_fac
	integer, intent(in)    :: itime
	integer, intent(in)    :: plnt_dayin
	real(8), intent(in)    :: tm
	real(8), intent(in)    :: sol_z( nsol )
	real(8), intent(in)    :: sol_tmp( nsol )
	integer, intent(in)    :: doy
	real(8), intent(in)    :: lati
	real(8), intent(in)    :: sr
	real(8), intent(in)    :: tx
	real(8), intent(in)    :: tn
	real(8), intent(in)    :: tn_next
	real(8), intent(in)    :: rh
	real(8), intent(in)    :: ca
	real(8), intent(in)    :: oa
	real(8), intent(in)    :: strsw
	real(8), intent(in)    :: ws
	real(8), intent(in)    :: uzz
	integer, intent(in)    :: year
	real(8), intent(in)    :: press
	real(8), intent(in)    :: sepbtm
	real(8), intent(in)    :: prec
	real(8), intent(in)    :: surfq
	real(8), intent(in)    :: sol_st( nsol )
	real(8), intent(in)    :: sol_fc( nsol )
	real(8), intent(in)    :: sol_wpmm( nsol )
	real(8), intent(in)    :: sol_bd( nsol )
	real(8), intent(in)    :: conv_wt( nsol )
	real(8), intent(in)    :: sol_prk( nsol )
	real(8), intent(in)    :: sol_ul( nsol )
	real(8), intent(in)    :: sol_clay( nsol )
	real(8), intent(in)    :: finp
	integer, intent(inout) :: crop_flag
	integer, intent(inout) :: non_dev
	real(8), intent(inout) :: gdd
	real(8), intent(inout) :: biomass
	real(8), intent(inout) :: leaf_bio
	real(8), intent(inout) :: soil_resid_l( nsol )
	real(8), intent(inout) :: cold_tm
	integer, intent(inout) :: cold_days
	real(8), intent(inout) :: froot
	real(8), intent(inout) :: tm_weighting(366)
	real(8), intent(inout) :: tm_preweight(366)
	integer, intent(inout) :: dae
	integer, intent(inout) :: lg_days
	real(8), intent(inout) :: sol_aorgn( nsol )
	real(8), intent(inout) :: sol_cbn( nsol )
	real(8), intent(inout) :: sol_fon( nsol )
	real(8), intent(inout) :: sol_fop( nsol )
	real(8), intent(inout) :: sol_no3( nsol )
	real(8), intent(inout) :: sol_orgn( nsol )
	real(8), intent(inout) :: sol_orgp( nsol )
	real(8), intent(inout) :: sol_rsd( nsol )
	real(8), intent(inout) :: sol_nh3( nsol )
	real(8), intent(inout) :: sol_actp( nsol )
	real(8), intent(inout) :: sol_solp( nsol )
	real(8), intent(inout) :: sol_stap( nsol )
	real(8), intent(inout) :: plantn
	real(8), intent(inout) :: plantp
	real(8), intent(inout) :: soilc(5)
	real(8), intent(out)   :: fgdd
	real(8), intent(out)   :: lai
	real(8), intent(inout) :: cht
	real(8), intent(out)   :: root_depth
	real(8), intent(out)   :: root_bio_l( nsol )
	real(8), intent(out)   :: ag_bio
	real(8), intent(out)   :: cold_strs
	real(8), intent(out)   :: yield
	real(8), intent(out)   :: strsn
	real(8), intent(out)   :: strsp
	real(8), intent(out)   :: totC
	real(8), intent(out)   :: co2_soil
	real(8), intent(out)   :: wdntl
	real(8), intent(out)   :: hmn_tot
	real(8), intent(out)   :: hmp_tot
	real(8), intent(out)   :: rmn_tot
	real(8), intent(out)   :: rmp_tot
	real(8), intent(out)   :: ga
	real(8), intent(out)   :: npp
	real(8), intent(out)   :: hr
	real(8), intent(out)   :: nep

	!----- declare statements of state variables -----
	real(8) :: strsall
	real(8) :: root_bio
	real(8) :: leaf_bio_save
	real(8) :: shoot_el_bio
	real(8) :: rd_root_c
	real(8) :: rd_shoot_c
	real(8) :: rd_g_all_c
	real(8) :: rd_root
	real(8) :: rd_shoot
	real(8) :: as
	real(8) :: cmass
	real(8) :: dbio
	real(8) :: ra
	real(8) :: cold_heat_change
	real(8) :: pinp
	integer :: covr
	real(8) :: defc
	real(8) :: depth
	real(8) :: rmtemp(4)
	real(8) :: rothc_d(4)
	real(8) :: ws_top
	real(8) :: n0
	real(8) :: leaf_bio_loss
	real(8) :: np_stress
	integer :: moist_rate

	!----- Others -----
	integer :: i_sol

	!----- Zero clear -----
	npp              = 0.0d0 ! Net primary production (µmol/m2/day)
	hr               = 0.0d0
	nep              = 0.0d0 ! Net ecosystem production (µmol/m2/day)
	if(itime==1) then
		crop_flag    = 0 ! If there is crop (1) or not (0)
		dae          = 0
		lg_days      = 0
		non_dev      = 0
		fgdd         = 0.0d0
		gdd          = 0.0d0
		lai          = 0.0d0
		biomass      = 0.0d0
		dbio         = 0.0d0
		cht          = 0.0d0
		root_depth   = 0.0d0
		root_bio_l   = 0.0d0
		root_bio     = 0.0d0
		ag_bio       = 0.0d0
		leaf_bio     = 0.0d0
		shoot_el_bio = 0.0d0
		as           = 0.0d0
		cmass        = 0.0d0
		yield        = -9999d0
	endif


	call s_param_regulation( ca )


	call s_planting( &
	tm,         &
	plnt_dayin, & !in    Planting day                                             (day)
	doy,        & !in    Day of years                                             (day)
	fgdd,       & !in    The rate of gdd to tgdd                                  (-)
	crop_flag,  & !inout There is crop (1) or not (0)                             (-)
	lg_days,    & !inout Leaf growth days                                         (-)
	biomass,    & !inout Biomass                                                  (kg dry/ha)
	gdd)          !inout Growth degree days                                       (deg C)

	call s_micro_met( &
	cht,     & !in  Canopy height                       (m)
	ws,      & !in  Wind speed at uzz                   (m/s)
	uzz,     & !in  Height at which wind speed measured (m)
	ws_top,  & !out Wind speed at the canopy top        (m/s)
	ga)        !out Aerodynamic conductance             (m/s)

	if( crop_flag == 1 ) then !IF #01 Start


		call s_development( &
		tm,        & !in    Average temperature                                    (deg C)
		non_dev,   & !inout continuous non-developing days                         (days)
		gdd,       & !inout Growing degree days                                    (deg C)
		fgdd)        !out   Fraction of GDD to total GDD needed for maturity (0-1) (-)

		call s_root_structure( &
		biomass,    & !in  Total biomass                                            (kg dry/ha)
		fgdd,       & !in  Fraction of gdd to total gdd needed for maturation (0-1) (-)
		nsol,       & !in  Number of soil layers                                    (-)
		sol_z,      & !in  Soil depth of the bottom of each layer                   (mm)
		root_depth, & !out Root depth                                               (mm)
		root_bio,   & !out Root biomass                                             (kg dry/ha)
		root_bio_l)   !out Root biomass of each layer                               (kg dry/ha)

		call s_shoot_structure( &
		lai_growth_type, & !in    Switch determinig LAI growth type                      (-)
		biomass,         & !in    Total biomass                                          (kg dry/ha)
		fgdd,            & !in    Fraction of GDD to total GDD needed for maturity (0-1) (-)
		lg_days,         & !in    Leaf growth day                                        (day)
		froot,           & !inout Root ratio                                             (-)
		cht,             & !out   Canopy height                                          (m)
		ag_bio,          & !out   Abobe ground biomass                                   (kg dry/ha)
		leaf_bio,        & !out   Leaf biomass                                           (kg dry/ha)
		shoot_el_bio,    & !out   Biomass of shoot except leaves                         (kg dry/ha)
		lai)               !out   LAI                                                    (ha/ha)

		call s_photosynthesis_big_leaf( &
    strs_fac,        & !in  Stress factor                                                                   (-)
		doy,             & !in  DOY                                                                             (day)
		lati,            & !in  Latitude                                                                        (deg)
		sr,              & !in  Daily global solor radiation                                                    (MJ/m2/day)
		tx,              & !in  Daily maximum temperature                                                       (deg C)
		tn,              & !in  Daily minimum temperature                                                       (deg C)
		tn_next,         & !in  Daily minimum temperature of the next day (for temperature cource estimation)   (deg C)
		rh,              & !in  Average relative humidity of the day                                            (%)
		ca,              & !in  Atmospheric CO2 consentration of the day                                        (ppm)
		oa,              & !in  Atmospheric O2 consentration of the day                                         (ppm)
		lai,             & !in  LAI of the day                                                                  (m2/m2)
		press,           & !in  Atmosphere pressure                                                             (Pa)
    ga,              & !in  Aerodynamic conductance                                                         (m/s)
		ws_top,          & !in  Wind speed at the top of canopy                                                 (m/s)
		as,              & !out Assimilation rate                                                               (É mol C/m2/day)
		cmass)             !out Assimilation rate                                                               (kg C/ha/day)

		call s_maintenance( &
		tm,              & !in  Average temperature                                            (deg C)
		nsol,            & !in  Number of soil layers                                          (-)
		sol_tmp,         & !in  Average temperature of each soil layer                         (deg C)
		root_bio_l,      & !in  Root biomass of each soil layer                                (kg dry/ha)
		ag_bio,          & !in  Above ground biomass                                           (kg dry/ha)
		rd_root_c,       & !out Maintenance respiration of root                                (kg C/ha/day)
		rd_shoot_c,      & !out Maintenance respiration of shoot                               (kg C/ha/day)
		rd_root,         & !out Maintenance respiration of root                                (kg dry/ha/day)
		rd_shoot) 	       !out Maintenance respiration of shoot                               (kg dry/ha/day)

		call s_calc_biomass( &
		lai_growth_type, & !in    Control switch
		cmass,           & !in    Assimilation rate                                        (kg C/ha/day)
		rd_root_c,       & !in    Root respiration                                         (kg C/ha/day)
		rd_shoot_c,      & !in    Respiration of shoot except leaves                       (kg C/ha/day)
		fgdd,            & !in    Fraction of GDD to total GDD needed for maturation (0-1) (-)
		lg_days,         & !in    Leaf growth day                                          (day)
		biomass,         & !inout Total biomass                                            (kg dry/ha)
		leaf_bio,        & !inout Leaf biomass                                             (kg dry/ha)
		leaf_bio_loss,   & !out   Leaf biomass loss                                        (kg dry/ha)
		rd_g_all_c,      & !out   Growth respiration                                       (kg C/ha/day)
		ag_bio,          & !out   Above ground biomass                                     (kg dry/ha)
		dbio)              !out   Biomass change                                           (kg dry/ha/day)

	endif !IF#01 End


	if( cons_swatnut == 1 ) then !IF#02 Start

		do i_sol = 1, nsol
			sol_rsd(i_sol) = sol_rsd(i_sol) + soil_resid_l(i_sol)
		enddo

		call s_swatnut(   &
		cons_rothc       , & !in    Consider RothC model (1) or not (0)                                 (-)
		soilc            , & !in    Soil carbon of each compartment                                     (t/ha)
		nsol             , & !in    Number of soil layers                                               (-)
		sol_tmp          , & !in    Soil temperature in each layer                                      (deg C)
		sol_st           , & !in    Amount of water in each soil layer at the day                       (mm)
		sol_fc           , & !in    Field capacity of each soil layer                                   (mm)
		sol_wpmm         , & !in    Wilting point of each soil layer                                    (mm)
		sol_z            , & !in    Length from the top of the soil to the botom of the each soil layer (mm)
		prec             , & !in    Precipitation depth (at the ground level)                           (mm)
		sepbtm           , & !in    Percolation from bottom of entire soil profile                      (mm)
		surfq            , & !in    Accumulated runoff or rainfall excess                               (mm)
		sol_bd           , & !in    Bulk density of each soil layer                                     (Mg/m^3)
		conv_wt          , & !in    Factor which converts kg/kg soil to kg/ha                           (-)
		fgdd             , & !in    Fraction of growing degree days to GDD needed for maturity          (rate)
		biomass          , & !in    Biomass of the plant                                                (kg dry/ha)
		dbio             , & !in    Biomass generated on current day                                    (kg dry/ha)
		root_depth       , & !in    Current rooting depth                                               (mm)
		sol_prk          , & !in    Percolation from each soil layer                                    (mm)
		sol_ul           , & !in    Water content of each layer at satulation                           (mm)
		sol_aorgn        , & !inout Amount of nitrogen stored in humus in each layer                    (kg/ha)
		sol_cbn          , & !inout Percent organic carbon in each soil layer                           (%)
		sol_fon          , & !inout Amount of organic N stored in the fresh        in each layer        (kg/ha)
		sol_fop          , & !inout Amount of organic P stored in the fresh        in each layer        (kg/ha)
		sol_no3          , & !inout Amount of mineral N stored in the nitrate pool in each layer        (kg/ha)
		sol_orgn         , & !inout Amount of organic N stored in the stable pool  in each layer        (kg/ha)
		sol_orgp         , & !inout Amount of organic P stored in the stable pool  in each layer        (kg/ha)
		sol_rsd          , & !inout Amount of organic matter in each soil layer                         (kg/ha)
		sol_nh3          , & !inout Amount of mineral N stored in the ammonium     in each layer        (kg/ha)
		sol_actp         , & !inout Amount of organic P stored in the active pool  in each layer        (kg/ha)
		sol_solp         , & !inout Amount of mineral P stored in the solution     in each layer        (kg/ha)
		sol_stap         , & !inout Amount of mineral P stored in the stable pool  in each layer        (kg/ha)
		plantn           , & !inout Amount of N in plant biomass                                        (kg/ha)
		plantp           , & !inout Amount of P in plant biomass                                        (kg/ha)
		strsn            , & !out   Nitrogen stress (0: heigh stress, 1: low stress)                    (-)
		strsp            , & !out   Phosphorus stress (0: heigh stress, 1: low stress)                  (-)
		wdntl,             & !out   Amount of nitrogen lost by denitrification                          (kg/ha)
		hmn_tot,           & !out   Amount of nitrogen mineralized from sol_aorgn in soil profile       (kg/ha)
		hmp_tot,           & !out   Amount of phosphorus mineralized from sol_orgp in soil profile      (kg/ha)
		rmn_tot,           & !out   Amount of nitrogen decomposed from sol_fon in soil profile          (kg/ha)
		rmp_tot            ) !out   Amount of phosphorus docomposed from sol_fop in soil profile        (kg/ha)

		if( cons_strsn == 1 ) np_stress = max( adjust_np_stress * strsp * strsn, 0.0d0 )
		if( cons_strsn == 0 ) np_stress = max( adjust_np_stress * strsp, 0.0d0 )
		if( np_stress > 1.0d0 ) np_stress = 1.0d0

		if( dbio >= 0.0d0 ) strsall =  min( strsw * np_stress, 1.0d0 )

	else

		if( dbio >= 0.0d0 ) strsall =  min(strsw, 1.0d0)

	endif !IF#02 End


	! Re-calculate biomass
	if( dbio >= 0.0d0 ) biomass = ( biomass - dbio ) + dbio * strsall
	if( dbio <  0.0d0 ) biomass = biomass

	npp = as * strsall - (rd_root_c + rd_shoot_c + rd_g_all_c)*1.0d3/1.0d4/14.0d0*1.0d6	! NPP (µ mol/m2/day)

	if( cons_rothc == 1 ) then !IF#03 Start

			covr = crop_flag
			defc = sol_fc(1) - sol_st(1)
			depth = sol_z(1)/10.0d0 ! from (mm) to (cm)
			pinp = c_rate*soil_resid_l(1)/1000.0d0 ! (t C/ha)

			call s_day_rothc(     &
			sol_tmp(1)        , & ! temperature
			pinp              , & ! plant C input
			finp              , & ! manure C input
			covr              , & ! plant cover
			sol_clay(1)       , & ! clay content
			depth             , & ! soil depth (cm)
			defc              , & ! water deficit (mm)
			soilc             , & ! SOC compartments (tC/ha)
			CO2_soil          , & ! released CO2 (tC/ha)
			totC              , & ! TSOC (tC/ha)
			rmtemp            , & ! Soil temperature factor
			rothc_d             & ! Decomposition rate
			)

			sol_cbn(1) = sum( soilc(1:5) ) / (sol_z(1)/1000.0d0 * sol_bd(1)*10000.0d0) * 100.0d0

			hr = CO2_soil*1.0d6/1.0d4/14.0d0*1.0d6 ! HR (É mol/m2/day)

	endif !IF#03 End

	soil_resid_l(:) = 0.0d0

	if( crop_flag == 1) then !IF#03 Start



		if( cold_heat_type /= 0 ) then

			call s_cold_heat_weight( &
      cold_heat_type, & !in type of stress (heat:1, cold:2)
			dae,          &   !in    Day after emergence
			fgdd,         &   !in    The rate of PHU to total PHU (rate 0-1)
			tx,           &   !in    Maximum temperature of the day
			tn,           &   !in    Minimum temperature of the day
			tm_weighting, &   !inout weight of temperature
			tm_preweight)     !inout temperature that will be weighted in subroutine 'cold_heat_stress'

			call s_cold_heat_stress( &
      cold_heat_type,     & !in type of stress (heat:1, cold:2)
			tm_weighting,       & !in weight of temperature
			tm_preweight,       & !in temperature that will be weighted in subroutine 'cold_heat_stress'
			cold_heat_change)     !out yield decrease due to cold stress type 1

		endif


		call s_harvest( &
		year,                & !in    Year
		nsol,                & !in    Number of soil layer                                     (-)
    doy,                 & !in    DOY                                                      (day)
		dae,                 & !inout The number of days after emergence                       (day)
		cold_heat_change,    & !inout Cold decrease type1                                      (kg/10a)
		cold_strs,           & !inout Cold stress factor                                       (-)
		non_dev,             & !inout continuous non-developing days                           (day)
		crop_flag,           & !inout There is crop (1) or not (0)                             (-)
		fgdd,                & !inout Fraction of GDD to total GDD needed for maturity (0-1)   (-)
		ag_bio,              & !inout Above ground biomass                                     (kg dry/ha)
		gdd,                 & !inout Growth degree days                                       (deg C)
		biomass,             & !inout Total biomass                                            (kg dry/ha)
		lai,                 & !inout LAI                                                      (ha/ha)
		cht,                 & !inout Canopy height                                            (m)
		root_depth,          & !inout Root depth                                               (mm)
		froot,               & !inout Root ratio                                               (-)
		root_bio_l,          & !inout Root biomass of each layer                               (kg dry/ha)
		soil_resid_l,        & !inout Residual in soil of each layer                           (kg dry/ha)
		leaf_bio,            & !inout Leaf biomass                                             (kg dry/ha)
		yield)                 !out   Yield                                                    (t dry/ha)

	else

		yield = -9999d0

	endif !IF#03 End


	nep = npp - hr ! NEP (É mol/m2/day)

end subroutine s_spm_crops

end module spm_crops
