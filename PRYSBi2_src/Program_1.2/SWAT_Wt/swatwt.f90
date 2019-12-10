module swatwt

use swatwt_param
use swatwt_param_fix

implicit none

private
public :: s_swatwt

contains

subroutine s_swatwt( &
	doy        , & !in    Day of Year
	tm         , & !in    Average temperature of the day (deg C)
	tx         , & !in    Maximum temperature of the day (deg C)
	tn         , & !in    Minimum temperature of the day (deg C)
	ta         , & !in    Average temperature over the simulation time (deg C)
	pr         , & !in    Precipitation of the day (mm)
	sr         , & !in    Solar radiation of the day (MJ/m^2)
	sx         , & !in    Maximum possible radiation of the day (MJ/m^2)
	rh         , & !in    Relative humidity (%)
	ws         , & !in    Wind speed (m/s)
	ca         , & !in    Atmospheric CO2 concentration (ppm)
	elev       , & !in    Elevation (m)
	slp        , & !in    Fraction of the slop of the ground (%)
	sol_alb    , & !in    Albed of each soil layer
	sol_fc     , & !in    Field capacity of each soil layer (mm)
	sol_ul     , & !in    Water content of each layer at satulation (mm)
	sol_shc    , & !in    Saturated hydraulic conductivity (mm/hr) 
	sol_z      , & !in    Length from the top of the soil to the botom of the each soil layer (mm)
	sol_sumfc  , & !in    Field capacity of the entire soil (mm)
	sol_sumul  , & !in    Water content of the entir soil at satulation (mm)
	sol_avbd   , & !in    Average albed of the soil
	lai        , & !in    LAI
	fgdd       , & !in    Fraction of growing degree days to GDD needed for maturity
	cht        , & !in    Canopy height (m)
	biomass    , & !in    Biomass of the plant (kg/ha)
	root_depth , & !in    Current rooting depth (mm)
	snow       , & !inout Amount of snow (mm)
	snotmp     , & !inout Snow temperature (deg C)
	canstor    , & !inout Water contained in canopy (mm)
	sol_st     , & !inout Amount of water in each soil layer at the day (mm)
	sol_no3    , & !inout Amonut of nitrogen strored in the nitrate pool in each soil layer (kg/ha)
	prec       , & !out   Precipitation depth (at the ground level) (mm)
	surfq      , & !out   Accumulated runoff or rainfall excess (mm)
	snofall    , & !out   Amount of snow falling (mm)
	snomlt     , & !out   Amount of snow melting (mm)
	albd       , & !out   Albedo
	ep_max     , & !out   Potential ET of plant (mm) (considering LAI)
	pet_day    , & !out   Potential ET (mm) (considering standerd 40cm alphalpha)
	sol_sw     , & !out   Amount of water stored in the entire soil profile (mm)
	es_day     , & !out   Actual amount of evaporation from soil (mm)
	ep_day     , & !out   Actual amount of transpiration (mm)
	sno3up     , & !out   Amount of nitrate moving upward in the soil (kg/ha)
	canev      , & !out   Amount of water evaporated from canopy (mm)
	snoev      , & !out   Amount of water in snow lost through sublimation (mm)
	sepbtm     , & !out   Percolation from bottom of entire soil profile (mm)
	sol_prk    , & !out   Percolation from each soil layer (mm)
	sol_tmp    , & !out   Soil temperature of each layer (deg C)
	et_day     , & !out   Actual amount of evapotranspiration (mm)
	strsw      )   !out   Water stress (0: heigh stress, 1: low stress)
	
	implicit none
	
	!----- Environment variable -----
	integer, intent(in)    :: doy
	real(8), intent(in)    :: tm
	real(8), intent(in)    :: tx
	real(8), intent(in)    :: tn
	real(8), intent(in)    :: ta
	real(8), intent(in)    :: pr
	real(8), intent(in)    :: sr
	real(8), intent(in)    :: sx
	real(8), intent(in)    :: rh
	real(8), intent(in)    :: ws
	real(8), intent(in)    :: ca
	real(8), intent(in)    :: elev
	real(8), intent(in)    :: slp
	real(8), intent(in)    :: sol_alb(nsol)
	real(8), intent(in)    :: sol_fc(nsol)
	real(8), intent(in)    :: sol_ul(nsol)
	real(8), intent(in)    :: sol_shc(nsol)
	real(8), intent(in)    :: sol_z(nsol)
	real(8), intent(in)    :: sol_sumfc
	real(8), intent(in)    :: sol_sumul
	real(8), intent(in)    :: sol_avbd
	real(8), intent(in)    :: lai
	real(8), intent(in)    :: fgdd
	real(8), intent(in)    :: cht
	real(8), intent(in)    :: biomass
	real(8), intent(in)    :: root_depth
	real(8), intent(inout) :: snow
	real(8), intent(inout) :: snotmp
	real(8), intent(inout) :: canstor
	real(8), intent(inout) :: sol_st(nsol)
	real(8), intent(inout) :: sol_no3(nsol)
	real(8), intent(out)   :: prec
	real(8), intent(out)   :: surfq
	real(8), intent(out)   :: snofall
	real(8), intent(out)   :: snomlt
	real(8), intent(out)   :: albd
	real(8), intent(out)   :: ep_max
	real(8), intent(out)   :: pet_day
	real(8), intent(out)   :: sol_sw
	real(8), intent(out)   :: es_day
	real(8), intent(out)   :: ep_day
	real(8), intent(out)   :: sno3up
	real(8), intent(out)   :: canev
	real(8), intent(out)   :: snoev
	real(8), intent(out)   :: sepbtm
	real(8), intent(out)   :: sol_prk(nsol)
	real(8), intent(out)   :: sol_tmp(nsol)
	real(8), intent(out)   :: et_day
	real(8), intent(out)   :: strsw



	!----- Substitute the amount of precipitation of the day (itime) ----
	prec = pr

	!----- Calculate about snow -----
	call Msnom( &
	doy       , & !in DOY
	tm        , & !in Daily average temperature
	tx        , & !in Daily maximum temperature
	snocov1   , & !in Shape parameter (calculated by ascrv)
	snocov2   , & !in Shape parameter (calculated by ascrv)
	snocovmx  , & !in Minimum amount of water for 100% snow cover (mm)
	sftmp     , & !in Snowing temperature (deg C)
	smtmp     , & !in Melting point (deg C)
	smfmx     , & !in Maximum melting speed (mm/deg C/day)
	smfmn     , & !in Minimum melting speed (mm/deg C/day)
	timp      , & !in The parameter deciding the lag of effect of temperature
	prec      , & !inout Precipitation (snow was subtracted)
	snow      , & !inout Amount of snow loaded (mm)
	snotmp    , & !inout Snow temperature (deg C)
	snofall   , & !out Snow falled (mm)
	snomlt    )   !out Melted snow (mm)


	!----- Calculate albedo -----
	call Malbedo( &
	sol_alb(1) , & !in Albedo of soil
	sol_cov    , & !in Residues of soil cover (kg/ha)
	lai        , & !in LAI
	snow       , & !in Amount of snow (mm)
	albd       )   !out Albedo


!	!----- Calculate canopy water ----- !2013/09/10d
!	call Mcalc_canopy( &
!	lai     , & !in LAI
!	mxlai   , & !in Maximum lai
!	canmx   , & !in Maximum water in canopy (mm)
!	prec    , & !inout Precipitation (mm) 
!	canstor )   !inout Water contained in canopy (mm)

	canstor = 0.0d0 !2013/09/10i

	!----- Calculate potential ET of plant, potential ET, and VPD -----
	call Metpot( &
	lai, &      !in LAI
	albd, &     !in Albedo
	cht, &      !in Canopy hight (m)
	ca, &       !in CO2 (ppmv)
	gsi, &      !in Maximum stmatal conductance (m/s)
	sr, &       !in Solar radiation (MJ/m^2)
	sx, &       !in Maximum possible radiation (calculated from lati)
	rh, &       !in Relative humidity
	snow, &     !in Amount of snow (mm)
	elev, &     !in Elevation (m)
	tm, &       !in Daily average temperature (deg C)
	ws, &       !in Wind speed (m/s)
	vpd2, &     !in Scale parameter determining CO2 effect on conductance
	ep_max, &   !out Potential ET of plant (mm)
	pet_day)    !out Potential ET (mm)


	!----- Calculate evaporation from canopy and soil, sublimation, and N movement (SWAT) -----
	call Metact( &
	esco, &      !in Soil evaporation compensation factor
	esd, &       !in Maximum soil depth for evaporation (mm)
	etco, &      !in Rate of water to be evaporating
	sol_cov, &   !in Residues of soil cover (kg/ha)
	biomass, &   !in Biomass (kg/ha)
	hru_dafr, &  !in Fraction of HRU
	pot_vol, &   !in Current volume of water stored in HRU (m^3)
	pet_day, &   !in Potential ET (mm)
	tm, &        !in Daily average temperature (deg C)
	nsol, &      !in No. of soil layers
	sol_fc, &    !in Amount of water available to plants in soil layer at fc (mm)
	sol_z, &     !in Depth to bottom of soil layer (mm)
	sol_st, &    !inout Amount of water in soil layer at the day (mm)
	sol_no3, &   !inout Amount of nitrogen stored in the soil layer (kg/ha)
	canstor, &   !inout Water contained in canopy (mm)
	ep_max, &    !inout Potential ET of plant (mm)
	snow, &      !inout Amount of snow (mm)
	sol_sw, &    !out Amount of water stored in the soil profile (mm)
	es_day, &    !out Actual amount of evaporation from soil (mm)
	sno3up, &    !out Amount of nitrate moving upward in the soil (kg/ha)
	canev, &     !out Amount of water evaporated from canopy (mm)
	snoev)       !out Amount of water in snow lost through sublimation (mm)


	!----- Calculate surface runoff -----
	call Msurfq( &
	prec,      & ! rainfall depth for the day (mm H2O)
	cn2,       & ! Moisture condition II curve number
	slp,       & ! The average fractio slpe of the subbasin (%)
	sol_sw,    & ! Soil water content of the entire profile excluding (mm H2O)
	sol_sumfc, & ! Amount of water in the soil profile at field capacity (mm H2O)
	sol_sumul, & ! Amount of water in the soil profile when completely saturated (mm H2O)
	sol_tmp(1),& ! Average temperature of soil layer (deg C)
	surfq)       ! Accumulated runoff or rainfall excess (mm H2O)


	!----- Calculate soil water -----
	call Mpercmain( &
	nsol, &    !in No. of soil layer
	surfq, &   !in Surface runoff generated on day (mm)
	prec, &    !in Precipitation (mm)
	sol_fc, &  !in Amount of water available to plants in soil layer at fc (mm)
	sol_ul, &  !in Water content at satulation (mm)
	sol_shc, & !in Saturated hydraulic conductivity (mm/hr)
	sol_tmp, & !in Average temperature of soil layer (deg C)
	sol_sw, &  !inout Amount of water stored in the soil profile (mm)
	sol_st, &  !inout Amount of water in soil layer at the day (mm)
	sepbtm, &  !out Percolation from bottom of soil profile (mm)
	sol_prk )  !out Percolation from soil layer (mm)


	!----- Calculate soil temperature -----
	call Msolt( &
	nsol, &      !in No. of soil layer
	sol_cov, &   !in Residues of soil cover (kg/ha)
	tlag, &      !in Lag coefficient for soil temperature
	tm,        & !in Daily average temperature
	tn,        & !in Daily minimum temperature
	tx,        & !in Daily maximum temperature
	ta,        & !in Yearly average temperature (calculated)
	sr,        & !in Solar radiation (MJ/m^2)
	snow, &      !in Amount of snow (mm)
	sol_sw, &    !in Amount of water stored in entire soil profile (mm)
	albd, &      !in Albedo
	sol_avbd, &  !in balk density (Mg/m^3)
	sol_z, &     !in Depth to bottom of soil layer (mm)
	biomass, &   !in Biomass (kg/ha)
	sol_tmp )    !out Average temperature of soil layer (deg C)


	!----- Calculate water stress (SWAT) -----
	call Mwstr( &
	fgdd, &       !in Rate of phu to tphu
	ep_max, &     !in Potential ET of plant (mm)
	nsol, &       !in No. of soil layer
	sol_fc, &     !in Amount of water available to plants in soil layer at fc (mm)
	sol_ul, &     !in Amount of water held in the soil layer at saturation (mm)
	sol_z, &      !in Depth to bottom of soil layer (mm)
	max_rd, &     !in Maximum root depth (mm)
	epco, &       !in Plant water uptake compensation factor (0-1)
	ubw, &        !in Water uptake distribution parameter
	root_depth, & !in Current rooting depth (mm)
	sol_st, &     !inout Amount of water in each soil layer at the day (mm)
	sol_sw, &     !inout Amount of water stored in the soil profile (mm)
	strsw, &      !out Water stress factor
	ep_day )      !out Actual amount of transpiration (mm)


	!----- Calculate ET (SWAT) -----
	et_day = ep_day + es_day + canev + snoev !::: ET = et of plant + et of soil + et of canopy

end subroutine s_swatwt
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Msnom(                          & !::: 
                  iida,                    & !::: In
                  tmpav,                   & !::: In
                  tmx,                     & !::: In
                  snocov1,                 & !::: In
                  snocov2,                 & !::: In
                  snocovmx,                & !::: In
                  sftmp,                   & !::: In
                  smtmp,                   & !::: In
                  smfmx,                   & !::: In
                  smfmn,                   & !::: In
                  timp,                    & !::: In
                  precipday,               & !::: Inout
                  sno_hru,                 & !::: Inout
                  snotmp,                  & !::: Inout
                  snofall,                 & !::: Out
                  snomlt                   & !::: Out
                  )                          !::: 

	implicit none
	
    integer , intent(in)    :: iida
    real(8) , intent(in)    :: tmpav
    real(8) , intent(in)    :: tmx
    real(8) , intent(in)    :: snocov1
    real(8) , intent(in)    :: snocov2
    real(8) , intent(in)    :: snocovmx
    real(8) , intent(in)    :: sftmp
    real(8) , intent(in)    :: smtmp
    real(8) , intent(in)    :: smfmx
    real(8) , intent(in)    :: smfmn
    real(8) , intent(in)    :: timp
    real(8) , intent(inout) :: precipday
    real(8) , intent(inout) :: sno_hru
    real(8) , intent(inout) :: snotmp
    real(8) , intent(out)   :: snofall
    real(8) , intent(out)   :: snomlt

    real(8) :: smfac, xx, snocov 

    ! estimate snow pack temperature
    snotmp = snotmp * ( 1.0d0 - timp ) + tmpav * timp

    if( tmpav <= sftmp ) then
      ! calculate snow fall
      sno_hru = sno_hru + precipday
      snofall = precipday
      precipday = 0.0d0
    else
      snofall = 0.0d0
    endif

    if( tmx > smtmp .and. sno_hru > 0.0d0 ) then
      ! adjust melt factor for time of year
      smfac = 0.0d0
      snomlt = 0.0d0
      smfac = ( smfmx + smfmn ) / 2.0d0 + sin( ( dble(iida) - 81.0d0 ) / &
		& 58.09d0 ) * ( smfmx - smfmn ) / 2.0d0      ! 365/2pi = 58.09
      snomlt = smfac * ( ( ( snotmp + tmx ) / 2.0d0 ) - smtmp )

      ! adjust for areal extent of snow cover
      if( sno_hru < snocovmx ) then
        xx = sno_hru / snocovmx
        snocov = xx / ( xx + exp( snocov1 - snocov2 * xx ) )
      else
        snocov = 1.0d0
      endif
      snomlt = snomlt * snocov
      if( snomlt < 0.0d0 ) snomlt = 0.0d0
      if( snomlt > sno_hru ) snomlt = sno_hru
      sno_hru = sno_hru - snomlt
      precipday = precipday + snomlt
      if( precipday < 0.0d0 ) precipday = 0.0d0
    else
      snomlt = 0.0d0
    end if

end subroutine Msnom
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Malbedo(                         & !::: 
                    sol_alb,                & !::: In
                    sol_cov,                & !::: In
                    laiday,                 & !::: In
                    sno_hru,                & !::: In
                    albday                  & !::: Out
                    )                         !::: 

	implicit none
	
    real(8) , intent(in)    :: sol_alb(1)
    real(8) , intent(in)    :: sol_cov
    real(8) , intent(in)    :: laiday
    real(8) , intent(in)    :: sno_hru
    real(8) , intent(out)   :: albday

    real(8) :: cej, eaj

    ! calculate albedo
    cej = -5.0d-5
    eaj = exp(cej * ( sol_cov + 0.1d0 ) )

    if( sno_hru <= 0.5d0 ) then
      albday = sol_alb(1)
      if( laiday > 0.0d0 ) albday = 0.23d0 * ( 1.0d0 - eaj ) + sol_alb(1) * eaj
     else
      albday = 0.8d0
    end if


end subroutine Malbedo
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Metpot( &
                   laiday,   & ! In LAI
                   albday,   & ! In Albedo
                   cht,      & ! In Canopy hight (m)
                   co2,      & ! In CO2 concentration (ppm)
                   gsi,      & ! In Maximum stmatal conductance of target plant (m/s)
                   sr,       & ! In Solar radiation (MJ/m2/day)
                   hru_rmx,  & ! In Maximum possible radiation (MJ/m2/day)
                   rhd,      & ! In Relative humidity (%: 0 - 100)
                   sno_hru,  & ! In Amount of snow (mm H20)
                   sub_elev, & ! In Elevation (m)
                   tmpav,    & ! In Daily average temperature (deg.C)
                   u10,      & ! In Wind speed at 10 m high (m/s)
                   vpd2,     & ! In Scale parameter determining CO2 effect on conductance (if unknown -9999.0d0)
                   ep_max,   & ! Out Potential Evapotranspiration (mm H20)
                   pet_day   & ! Out Potential Evapotranspiration of Alfalfa 40cm
                   )

	implicit none
	
    real(8) , intent(in)  :: laiday
    real(8) , intent(in)  :: albday
    real(8) , intent(in)  :: cht
    real(8) , intent(in)  :: co2
    real(8) , intent(in)  :: gsi
    real(8) , intent(in)  :: sr
    real(8) , intent(in)  :: hru_rmx
    real(8) , intent(in)  :: rhd
    real(8) , intent(in)  :: sno_hru
    real(8) , intent(in)  :: sub_elev
    real(8) , intent(in)  :: tmpav
    real(8) , intent(in)  :: u10
    real(8) , intent(in)  :: vpd2
    real(8) , intent(out) :: ep_max
    real(8) , intent(out) :: pet_day

    integer :: j
    real(8) :: tk, pb, gma, xl, ea, ed, dlt, ralb1, ralb, xx
    real(8) :: rbo, rto, rn, uzz, zz, zom, zov, rv, rn_pet, fvpd
    real(8) :: rc, rho, rout, d, chz, gsi_adj, u, tmpc, vpd_coef
    real(8) :: vpd, ga

    !----- initialize local variables -----
	u = u10
    tk = tmpav + 273.15d0 !from deg. C to K
	vpd_coef = vpd2
	if(vpd2 == -9999.0d0) vpd_coef = 0.08333333d0

    ! calculate mean barometric pressure (kPa)
    pb = 101.3d0 - sub_elev * ( 0.01152d0 - 0.544d-6 * sub_elev )

    ! calculate latent heat of vaporization (MJ/kg)
    xl = 2.501d0 - 2.361d-3 * tmpav

    ! calculate psychrometric constant (kPa/deg.C)
    gma = 1.013d-3 * pb / ( 0.622d0 * xl )

    ! calculate saturation vapor pressure (kPa) Eq. 1;3.5.2 (2009)
    ea = ( 16.78d0 * tmpav - 116.9d0 ) / ( tmpav + 237.3d0 )
    ea = exp(ea)                                            

	! actual vapor pressure (kPa)
    ed = ea * rhd / 100.0d0

    ! vapor pressure deficit (kPa)
    vpd = ea - ed


    ! calculate the slope of the saturation vapor pressure curve (kPa/deg.C)
    dlt = 4098.0d0 * ea / ( tmpav + 237.3d0 )**2.0d0

    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    !::: DETERMINE POTENTIAL ET                              :::
    !::: PENMAN-MONTEITH POTENTIAL EVAPOTRANSPIRATION METHOD :::
    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    !----- calculate net radiation -----

    !--- calculate net short-wave radiation for PET ---
    if( sno_hru <= 0.5d0 ) then
      ralb = sr * ( 1.0d0 - 0.23d0 )
    else
      ralb = sr * ( 1.0d0 - 0.8d0 ) ! Eq. 1;1.2.13 (2009)
    end if


    !--- calculate net short-wave radiation for max plant ET (MJ/m2/day)---
    ralb1 = sr * ( 1.0d0 - albday )


    !--- calculate net long-wave radiation ---
    ! net emissivity equation eq. 1:1.2.22
    rbo = -( 0.34d0 - 0.139d0 * sqrt(ed) )

    ! cloud cover factor equation eq. 1:1.2.22
    rto = 0.9d0 * ( sr / hru_rmx ) + 0.1d0

    ! net long-wave radiation (MJ/m2/day) eq. 1:1.2.22
    rout = rbo * rto * 4.903d-9 * (tk**4)


    !--- calculate net radiation ---
    rn = ralb1 + rout
    rn_pet = ralb + rout

    !! 2:2.2.19 (2009)
    rho = 1710.0d0 - 6.85d0 * tmpav

    if ( u < 0.01d0 ) u = 0.01d0


    !!! potential ET: reference crop alfalfa at 40 cm height
    rv = 114. / (u * (170./1000.)**0.2)
    rc = 49.0d0 / ( 1.4d0 - 0.4d0 * co2 / 330d0 )

    pet_day = ( dlt * rn_pet + gma * rho * vpd / rv ) / ( xl * ( dlt + gma * ( 1.0d0 + rc / rv ) ) )
    pet_day = max( 0.0d0, pet_day )


    !!! maximum plant ET
    if( laiday <= 0.0d0 ) then
      ep_max = 0.0d0

      if( cht <= 1.0d0 ) then
        uzz = u ! wind speed (m/s)
        zz = 170.0d0 ! height at which wind speed determined (cm)
      else
        zz = cht * 100.0d0 + 100.0d0
        uzz = u * ( zz / 170.0d0 )**0.2d0
      endif

      !! calculate canopy height in cm
      if ( cht < 0.01d0 ) then
        chz = 1.0d0
      else
        chz = cht * 100.0d0
      endif

      !! calculate roughness length for momentum transfer
      if( chz <= 200.0d0 ) then
        zom = 0.123d0 * chz
      else
        zom = 0.058d0 * chz**1.19d0
      endif


      !! calculate roughness length for vapor transfer
      zov = 0.1d0 * zom

      !! calculate zero-plane displacement of wind profile
      d = 0.667d0 * chz

      !! calculate aerodynamic resistance
      rv = log( ( zz - d ) / zom ) * log( ( zz - d ) / zov )
      rv = rv / ( ( 0.41d0 )**2 * uzz )
	  ga = 1.0d0/rv

    else

      !! determine wind speed (m/s) and height of wind speed measurement
      !! adjust to 100 cm (1 m) above canopy if necessary
      if (cht <= 1.0) then
        zz = 170.0
       else
        zz = cht * 100. + 100.
      end if
      uzz = u10 * (zz/1000.)**0.2

      !! calculate canopy height in cm
      if ( cht < 0.01d0 ) then
        chz = 1.0d0
      else
        chz = cht * 100.0d0
      endif

      !! calculate roughness length for momentum transfer
      if( chz <= 200.0d0 ) then
        zom = 0.123d0 * chz
      else
        zom = 0.058d0 * chz**1.19d0
      endif


      !! calculate roughness length for vapor transfer
      zov = 0.1d0 * zom

      !! calculate zero-plane displacement of wind profile
      d = 0.667d0 * chz

      !! calculate aerodynamic resistance
      rv = log( ( zz - d ) / zom ) * log( ( zz - d ) / zov )
      rv = rv / ( ( 0.41d0 )**2 * uzz )
	  ga = 1.0d0/rv

      !! adjust stomatal conductivity for low vapor pressure
      !! this adjustment will lower maximum plant ET for plants
      !! sensitive to very low vapor pressure
      xx = vpd - 1.0d0
      if( xx > 0.0d0 ) then
        fvpd = max( 0.1d0, 1.0d0 - vpd_coef * xx )
      else
        fvpd = 1.0d0
      end if
      gsi_adj = gsi * fvpd

      !! calculate canopy resistance (s/m)
      rc = 1.0d0 / gsi_adj !single leaf resistance (s/m)
!      rc = rc / ( 0.5d0 * ( laiday + 0.01d0 ) * ( 1.4d0 - 0.4d0 * co2 / tmpc ) ) !2013/09/10d
      rc = rc / ( 0.5d0 * ( laiday + 0.01d0 ) * ( 1.4d0 - 0.4d0 * co2 / 330.0d0 ) ) !2013/09/10i

      !! calculate maximum plant ET
      ep_max = ( dlt * rn + gma * rho * vpd / rv ) / ( xl * ( dlt + gma * ( 1.0d0 + rc / rv ) ) )
      if( ep_max < 0.0d0 ) then
		ep_max = 0.0d0
	  endif

	  ep_max = min(ep_max, pet_day)

    endif

end subroutine Metpot
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mcalc_canopy(                              & !::: 
                         lai,                         & !::: In
                         mxlai,                       & !::: In
                         canmx,                       & !::: In
                         precipday,                   & !::: Inout
                         canstor)                       !::: Inout

	implicit none
	
	real(8) , intent(in)    :: lai
	real(8) , intent(in)    :: mxlai
	real(8) , intent(in)    :: canmx
	real(8) , intent(inout) :: precipday
	real(8) , intent(inout) :: canstor

	real(8) :: flai, xx, canmxl

	!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	!::: this subroutine computes canopy interception of rainfall :::
	!::: used for methods other than curve number                 :::
	!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

!	flai = fgdd / ( fgdd + exp( leaf1 - leaf2 * fgdd ) )
	flai = min(lai / mxlai, 1d0)

	xx = precipday
	canmxl = canmx * flai
	precipday = precipday - ( canmxl - canstor )
	if( precipday < 0.0d0 ) then
		canstor = canstor + xx
		precipday = 0.0d0
	else
		canstor = canmxl
	endif

end subroutine Mcalc_canopy
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Metact(            & ! 
                   esco,      & ! In
                   esd,       & ! In
                   etco,      & ! In
                   sol_cov,   & ! In
				   biomass,   & ! In
                   hru_dafr,  & ! In
                   pot_vol,   & ! In
                   pet_day,   & ! In
                   tmpav,     & ! In
                   nsol,      & ! In
                   sol_fc,    & ! In
                   sol_z,     & ! In
                   sol_st,    & ! Inout
                   sol_no3,   & ! Inout
                   canstor,   & ! Inout
                   ep_max,    & ! Inout
                   sno_hru,   & ! Inout
                   sol_sw,    & ! Out
                   es_day,    & ! Out
                   sno3up,    & ! Out
                   canev,     & ! Out
                   snoev      & ! Out
                   )

	implicit none
	
    real(8), intent(in)    :: esco
    real(8), intent(in)    :: esd
    real(8), intent(in)    :: etco
    real(8), intent(in)    :: sol_cov
	real(8), intent(in)    :: biomass
    real(8), intent(in)    :: hru_dafr
    real(8), intent(in)    :: pot_vol
    real(8), intent(in)    :: pet_day
    real(8), intent(in)    :: tmpav
    integer, intent(in)    :: nsol
    real(8), intent(in)    :: sol_fc(nsol)
    real(8), intent(in)    :: sol_z(nsol)
    real(8), intent(inout) :: sol_st(nsol)
    real(8), intent(inout) :: sol_no3(nsol)
    real(8), intent(inout) :: canstor
    real(8), intent(inout) :: ep_max
    real(8), intent(inout) :: sno_hru
    real(8), intent(out)   :: sol_sw
    real(8), intent(out)   :: es_day
    real(8), intent(out)   :: sno3up
    real(8), intent(out)   :: canev
    real(8), intent(out)   :: snoev

    integer :: ib, ly
    real(8) :: no3up, es_max, eos1, xx, cej, eaj, pet, esleft
    real(8) :: evzp, eosl, dep, evz, sev, expo


    !! evaporate canopy storage first
    !! canopy storage is calculated by the model only if the Green & Ampt
    !! method is used to calculate surface runoff. The curve number methods
    !! take canopy effects into account in the equations. For either of the
    !! CN methods, canstor will always equal zero.

	!----- Evaporation from canopy -----
    pet = pet_day
    pet = pet - canstor ! evaporate canopy storage

    if( pet < 0.0d0 ) then ! If PET is less than the amount of water stored in canopy,
		canstor = -pet
		canev = pet_day
		pet = 0.0d0
		ep_max = 0.0d0
		es_max = 0.0d0
    else
		canev = canstor
		canstor = 0.0d0
    endif

    !----- Compute potential soil evaporation -----
    cej = -5.0d-5
    if( sno_hru >= 0.5d0 ) then ! If there is snow on the cover
		  eaj = 0.5d0 ! Waiting factor
    else
		  eaj = exp( cej * ( sol_cov + biomass + 0.1d0 ) ) ! Waiting factor increases with residuals.
    end if

	! Calculate 'es_max' according to eaj that is calculated status of cover of soil.
    es_max = pet * eaj
    eos1 = pet / ( es_max + ep_max + 1.0d-10 )
    eos1 = es_max * eos1
    es_max = min( es_max, eos1 )
    es_max = max( es_max, 0.0d0 )
    if( pot_vol > 1.0d-4 ) es_max = 0.0d0 ! pot_vol = 0.0d0 (fixed)

    ! make sure maximum plant and soil ET doesn't exceed potential ET
    if (pet_day < es_max + ep_max) then
      es_max = pet_day - ep_max
      if (pet < es_max + ep_max) then
        es_max = pet * es_max / (es_max + ep_max)
        ep_max = pet * ep_max / (es_max + ep_max)
      end if
    end if

	!----- Calculate actual soil evaporation -----
    ! initialize soil evaporation variables
    esleft = es_max

	!----- Evaporation from snow -----
    ! compute sublimation without elevation bands
    snoev = 0.
    if( tmpav > 0.0d0 ) then
		if( sno_hru >= esleft ) then
			! take all soil evap from snow cover
			sno_hru = sno_hru - esleft
			snoev = esleft
			esleft = 0.0d0
		else
			if( sno_hru > 0.0d0 ) then
				! take all soil evap from snow cover then start taking from soil
				esleft = esleft - sno_hru
				snoev = sno_hru
				sno_hru = 0.0d0
			else
				esleft = esleft
				snoev = 0.0d0
				sno_hru = 0.0d0
			end if
		endif
    endif

	!----- Evaporation from soil -----
    ! take soil evap from each soil layer
    eosl = esleft
	evzp = 0.0d0
    do ly = 1, nsol

      ! depth exceeds max depth for soil evap (esd)
      if( ly == 1 ) then
        dep = sol_z(1)
      else
        dep = sol_z(ly-1)
      endif

      if( dep < esd ) then
        ! calculate evaporation from soil layer
		! evz = evaporative demand at depth z (mm H20)
        evz = eosl * sol_z(ly) / (sol_z(ly) + exp( 2.374d0 - 0.00713d0 * sol_z(ly) ) )
        sev = evz - evzp * esco
        evzp = evz
        if( sol_st(ly) < sol_fc(ly) ) then
          xx =  2.5d0 * ( sol_st(ly) - sol_fc(ly) ) / sol_fc(ly)
          if( xx < -20.0d0 ) xx = -20.0d0
          if( xx > 20.0d0 ) xx = 20.0d0
          expo = exp(xx)       
          sev = sev * expo
        end if
        sev = min( sev, sol_st(ly) * etco )

        if( sev < 0.0d0 ) sev = 0.0d0
        if( sev > esleft ) sev = esleft

        ! adjust soil storage, potential evap
        if( sol_st(ly) > sev ) then
          esleft = esleft - sev
          sol_st(ly) = max( 1.0d-6, sol_st(ly) - sev )
        else
          esleft = esleft - sol_st(ly)
          sol_st(ly) = 0.0d0
        endif
      endif

      !----- compute no3 flux from layer 2 to 1 by soil evaporation -----
	  ! p. 189 in SWAT man 2005
      if( ly == 2 ) then
        no3up = 0.1d0 * sev * sol_no3(2) / ( sol_st(2) + 1.0d-6 )
		!notice original is 0.1d0 but it would be bug.

        no3up = min( no3up, sol_no3(2) )
        sno3up = no3up * hru_dafr
        sol_no3(2) = sol_no3(2) - no3up
        sol_no3(1) = sol_no3(1) + no3up
      endif

    end do

    !----- update total soil water content -----
    sol_sw = 0.0d0
    do ly = 1, nsol
      sol_sw = sol_sw + sol_st(ly)
    end do

    !----- calculate actual amount of evaporation from soil -----
    es_day = es_max - esleft
    if( es_day < 0.0d0 ) es_day = 0.0d0

end subroutine Metact
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Msurfq( &
	prec,      & ! rainfall depth for the day (mm H2O)
	cn2,       & ! Moisture condition II curve number
	slp,       & ! The average fractio slpe of the subbasin (%)
	sol_sw,    & ! Soil water content of the entire profile excluding (mm H2O)
	sol_sumfc, & ! Amount of water in the soil profile at field capacity (mm H2O)
	sol_sumul, & ! Amount of water in the soil profile when completely saturated (mm H2O)
	sol_tmp,   & ! Average temperature of soil layer (deg C)
	surfq)       ! Accumulated runoff or rainfall excess (mm H2O)

	implicit none
	
	real(8), intent(in)  :: prec
	real(8), intent(in)  :: cn2
	real(8), intent(in)  :: slp
	real(8), intent(in)  :: sol_sw
	real(8), intent(in)  :: sol_sumfc
	real(8), intent(in)  :: sol_sumul
	real(8), intent(in)  :: sol_tmp
	real(8), intent(out) :: surfq


	real(8) :: cn1, cn3, cn2s
	real(8) :: reten1, reten3, reten
	real(8) :: w1, w2
	real(8) :: x1, x2, x3
        real(8) :: cn_froz


	cn1 = cn2 - 20d0*(100d0-cn2) / (100d0 - cn2 + exp(2.533d0 - 0.0636d0*(100d0-cn2)))
	cn3 = cn2*exp(0.00673d0*(100d0-cn2))

	!--- Considering slop of the ground ---
	cn2s = (cn3-cn2)/3d0 * (1d0 - 2d0*exp(-13.86d0*slp*0.01d0)) + cn2
	cn1 = cn2s - 20d0*(100d0-cn2s) / (100d0 - cn2s + exp(2.533d0 - 0.0636d0*(100d0-cn2s)))
	cn3 = cn2s*exp(0.00673d0*(100d0-cn2s))

	!--- Calculate retention parameter ---
	reten1 = 25.4d0 * ( 1000d0/cn1 - 10d0 )
	reten3 = 25.4d0 * ( 1000d0/cn3 - 10d0 )

	w2 = (log(sol_sumfc/(1d0-reten3/reten1) - sol_sumfc) - log(sol_sumul/(1d0-2.54d0/reten1) - sol_sumul)) / (sol_sumul - sol_sumfc)
	w1 = log(sol_sumfc/(1d0-reten3/reten1) - sol_sumfc) + w2*sol_sumfc

	reten = reten1 * (1d0 - sol_sw / (sol_sw + exp(w1 - w2*sol_sw)))

        !! calculate retention parameter when the top layer is frozen (eq. 2:1.1.10)
        cn_froz = 0.000862
        if (sol_tmp <= 0.) reten = reten1 * (1. - Exp(- cn_froz * reten))

	surfq = 0.0
        if(prec-0.2*reten.gt.0.) then
          surfq = (prec - 0.2d0*reten)**2 / (prec + 0.8d0*reten)
        end if

end subroutine Msurfq
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mpercmain(            & ! 
                      nsol,      & ! In
                      surfq,     & ! In
                      precipday, & ! In
                      sol_fc,    & ! In
					  sol_ul,    & ! In
					  sol_shc,   & ! In
                      sol_tmp,   & ! In
                      sol_sw,    & ! Inout
                      sol_st,    & ! Inout
                      sepbtm,    & ! Out
                      sol_prk    & ! Out
                      )

	! In this subroutine 'water content' meanst the content of water that was subtracted by 
	! Wilting point(mm).
	! In this submodel the other water frow other than bottom percolation was not considere
	! for large-scale simulation.

	implicit none
	
	integer , intent(in)              :: nsol ! The number of soil layer
	real(8) , intent(in)    :: surfq ! surface runoff(mm)
	real(8) , intent(in)    :: precipday ! Precipitation(mm)
	real(8) , intent(in)    :: sol_fc(nsol) ! water content in the layer at field capacity(mm)
	real(8) , intent(in)    :: sol_ul(nsol) ! water content in satulation(mm)
	real(8) , intent(in)    :: sol_shc(nsol) ! saturated hydraulic conductivity (mm/h)
	real(8) , intent(in)    :: sol_tmp(nsol) ! Average temperature of soil layer (deg C)
	real(8) , intent(inout) :: sol_sw ! Water content in the soil(mm)
	real(8) , intent(inout) :: sol_st(nsol) ! Water content in the layer(mm)
	real(8) , intent(out)   :: sepbtm ! percolation from bottom layter(mm)
	real(8) , intent(out)   :: sol_prk(nsol) ! percolation from the layer(mm)

	integer  :: i
	real(8)  :: inflpcp, sepday, sw_excess, tt_perc(nsol), sw_over

	!! Compute effective rainfall (amount that percs into soil)
	inflpcp = precipday - surfq

	if( inflpcp < 0.0d0 ) inflpcp = 0.0d0

	!! initialize water entering first soil layer
	sepday = inflpcp
	sepbtm = 0.0d0
	sol_prk = 0.0d0
	tt_perc = 0.0d0
	sw_over = 0.0d0

	do i = 1, nsol
		!! add water moving into soil layer from overlying layer
		sol_st(i) = sol_st(i) + sepday
		tt_perc(i) = (sol_ul(i) - sol_fc(i))/sol_shc(i)

		!! determine gravity drained water in layer

		if( sol_st(i) > sol_fc(i) ) then
			sw_excess = sol_st(i) - sol_fc(i)
			sepday = sw_excess*(1.0d0 - exp(-24.0d0/tt_perc(i)))
                        if (sol_tmp(i) <= 0.) then
                          sepday = 0.
                        end if
			sol_st(i) = sol_st(i) - sepday
			sepday    = sepday + max(sol_st(i) - sol_ul(i), 0.0d0) !2013/09/10i
			if(sol_st(i) > sol_ul(i)) sol_st(i) = sol_ul(i)
		else
			sepday = 0.0d0
		end if


		!! summary calculations
		sol_prk(i) = sepday

		if( i == nsol ) sepbtm = sepday
	end do


	!! update soil profile water
	sol_sw = 0.0d0
	do i = 1, nsol
		sol_sw = sol_sw + sol_st(i)
	end do

end subroutine Mpercmain
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Msolt(                           & ! 
                  nsol,                     & ! In
                  sol_cov,                  & ! In
                  tlag,                     & ! In
                  tmpav,                    & ! In
                  tmn,                      & ! In
                  tmx,                      & ! In
                  tmp_an,                   & ! In
                  sr,                       & ! In
                  sno_hru,                  & ! In
                  sol_sw,                   & ! In
                  albday,                   & ! In
                  sol_avbd,                 & ! In
                  sol_z,                    & ! In
				  biomass,                  & ! In
                  sol_tmp                   & ! Inout
                  )                           ! 

	implicit none
	
    integer ,           intent(in)    :: nsol
    real(8) , intent(in)    :: sol_cov
    real(8) , intent(in)    :: tlag
    real(8) , intent(in)    :: tmpav
    real(8) , intent(in)    :: tmn
    real(8) , intent(in)    :: tmx
    real(8) , intent(in)    :: tmp_an
    real(8) , intent(in)    :: sr
    real(8) , intent(in)    :: sno_hru
    real(8) , intent(in)    :: sol_sw
    real(8) , intent(in)    :: albday
    real(8) , intent(in)    :: sol_avbd
    real(8) , intent(in)    :: sol_z(nsol)
	real(8) , intent(in)    :: biomass
    real(8) , intent(inout) :: sol_tmp(nsol)

    integer :: j, k
    real(8) :: f, dp, ww, b, wc, dd, xx, st0
    real(8) :: df, zd, bcv, tbare, tcov, tmp_srf

    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    !::: this subroutine estimates daily average temperature at the bottom
    !::: of each soil layer
    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    !! calculate damping depth
    !! calculate maximum damping depth
    !! SWAT manual equation 2.3.6

    f = sol_avbd / ( sol_avbd + 686.0d0 * exp( -5.63d0 * sol_avbd ) )
    dp = 1000.0d0 + 2500.0d0 * f

    !! calculate scaling factor for soil water
    !! SWAT manual equation 2.3.7
    ww = 0.356d0 - 0.144d0 * sol_avbd
    wc = sol_sw / ( ww * sol_z(nsol) )

    !! calculate daily value for damping depth
    !! SWAT manual equation 2.3.8
    b = log( 500.0d0 / dp )
    f = exp( b * ( ( 1.0d0 - wc ) / ( 1.0d0 + wc ) )**2.0d0 )
    dd = f * dp

    !! calculate lagging factor for soil cover impact on soil surface temp
    !! SWAT manual equation 2.3.11
    bcv = (sol_cov+biomass) / ( (sol_cov+biomass) + exp( 7.563d0 - 1.297d-4 * (sol_cov+biomass) ) )
    if( sno_hru > 0.0d0 ) then
      if( sno_hru.le.120.0d0 ) then
        xx = 0.0d0
        xx = sno_hru / ( sno_hru + exp( 6.055d0 - 0.3002d0 * sno_hru ) )
      else
        xx = 1.0d0
      end if
      bcv = max( xx, bcv )
    end if

    !! calculate temperature at soil surface
    !! SWAT manual equation 2.3.10
    st0 = ( sr * ( 1.0d0 - albday ) - 14.0d0 ) / 20.0d0

    !! SWAT manual equation 2.3.9
    tbare = tmpav + 0.5d0 * ( tmx - tmn ) * st0

    !! SWAT manual equation 2.3.12
    tcov = bcv * sol_tmp(2) + ( 1.0d0 - bcv ) * tbare

    if( (sol_cov+biomass) > 0.01d0 .or. sno_hru > 0.01d0 ) then
      tmp_srf = min( tbare, tcov )
    else
      tmp_srf = tbare
    end if

    !! calculate temperature for each layer on current day
    xx = 0.
    do k = 1, nsol
      zd = ( xx + sol_z(k) ) / 2.0d0              !! calculate depth at center of layer
      zd = zd / dd                                !! SWAT manual equation 2.3.5
      !! SWAT manual equation 2.3.4
      df = zd / ( zd + exp( -0.8669d0 - 2.0775d0 * zd ) )
      !! SWAT manual equation 2.3.3
      sol_tmp(k) = tlag * sol_tmp(k) + ( 1.0d0 - tlag ) * ( df * ( tmp_an - tmp_srf) + tmp_srf )
      xx = sol_z(k)
    end do

end subroutine Msolt
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mwstr(                           & !::: 
                  phuacc,                   & !::: In
                  ep_max,                   & !::: In
                  nsol,                     & !::: In
                  sol_fc,                   & !::: In
                  sol_ul,                   & !::: In
                  sol_z,                    & !::: In
                  max_rd,                   & !::: In
                  epco,                     & !::: In
                  ubw,                      & !::: In
                  root_depth,               & !::: In
                  sol_st,                   & !::: Inout
                  sol_sw,                   & !::: Inout
                  strsw,                    & !::: Out
                  ep_day                    & !::: Out
                  )                           !::: 

	implicit none

    real(8) , intent(in)    :: phuacc
    real(8) , intent(in)    :: ep_max
    integer , intent(in)             :: nsol
    real(8) , intent(in)    :: sol_fc(nsol)
    real(8) , intent(in)    :: sol_ul(nsol)
    real(8) , intent(in)    :: sol_z(nsol)
    real(8) , intent(in)    :: max_rd
    real(8) , intent(in)    :: epco
    real(8) , intent(in)    :: ubw
    real(8) , intent(in)    :: root_depth
    real(8) , intent(inout) :: sol_st(nsol)
    real(8) , intent(inout) :: sol_sw
    real(8) , intent(out)   :: strsw
    real(8) , intent(out)   :: ep_day

    integer :: k, ir
    real(8) :: uobw, wuse(nsol), sum, xx, gx, reduc, ul4, sump
    real(8) :: sol_sumfc, sol_sumul, satco, strsa, yy

    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    !::: this subroutine distributes potential plant evaporation through   :::
    !::: the root zone and calculates actual plant water use based on soil :::
    !::: water availability. Also estimates water stress factor.           :::
    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

	uobw = 1.0d0 - exp(-ubw)

!    root_depth = 2.5d0 * phuacc * max_rd
!    if( root_depth.gt.max_rd ) root_depth = max_rd
!    if( root_depth.lt.10.0d0 ) root_depth = 10.0d0

    if( ep_max.le.0.01d0 ) then
      strsw = 1.0d0
      ep_day = 0.0d0
    else
      if( phuacc.eq.0.0d0 ) then
        strsw = 1.0d0
        ep_day = 0.0d0
      else
        !!  compute aeration stress
        sol_sumfc = 0.0d0
        sol_sumul = 0.0d0
        do k = 1, nsol
          sol_sumfc = sol_sumfc + sol_fc(k)
          sol_sumul = sol_sumul + sol_ul(k)
        end do

        if( sol_sw.gt.sol_sumfc ) then
          satco = ( sol_sw - sol_sumfc ) / ( sol_sumul - sol_sumfc )
          strsa = 1.0d0 - ( satco / ( satco + exp( 0.176d0 - 4.544d0 * satco ) ) )
        else
          strsa = 1.0d0
        end if

        ir = 0
        xx = 0.0d0
        sump = 0.0d0

        do k = 1, nsol
          if( ir.gt.0 ) exit

          if( root_depth.le.sol_z(k) ) then
            gx = root_depth
            ir = k
          else
            gx = sol_z(k)
          end if

          if( root_depth.le.0.01d0 ) then
            sum = ep_max / uobw
          else
            sum = ep_max * ( 1.0 - exp( -ubw * gx / root_depth ) ) / uobw
          end if

          !! don't allow compensation for aeration stress
!          if( strsa.gt.0.99d0 ) then
!            yy = 0.0d0
!          else
!            yy = sump - xx
!          end if
!          wuse(k) = sum - sump + yy * epco
          wuse(k) = sum - sump + (sump - xx) * epco
          sump = sum

          !! adjust uptake if sw is less than 25% of plant available water
          if( sol_st(k).lt.sol_fc(k)/4.0d0 ) then
            reduc = exp( 5.0d0 * ( 4.0d0 * sol_st(k) / sol_fc(k) - 1.0d0 ) )
          else
            reduc = 1.0d0
          endif
          wuse(k) = wuse(k) * reduc

          if( sol_st(k).lt.wuse(k) ) then
            wuse(k) = sol_st(k)
          end if

          sol_st(k) = max( 1.d-6, sol_st(k) - wuse(k) )
          xx = xx + wuse(k)
        end do

        !! update total soil water in profile
        sol_sw = 0.0d0
        do k = 1, nsol
          sol_sw = sol_sw + sol_st(k)
        end do

        strsw = strsa * xx / ep_max
        if( strsw.lt.0.0d0 ) strsw = 0.0d0

        ep_day = xx
      end if

    end if

	if(strsw /= 1.0d0) then
		strsw = strsw
		if(strsw <= 0.0d0) strsw = 0.0d0
		if(strsw >= 1.0d0) strsw = 1.0d0
	endif

!	strsw = 1.0d0 - strsw

	if( phuacc == 0.0d0 ) ep_day = 0.0d0 !GS

end subroutine Mwstr
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mascrv(                        & !:::
                 x1, x2, x3, x4,          & !::: In
                 x5, x6                   & !::: Out
                 )                          !:::

	implicit none
	real(8) :: xx

	real(8), intent (in)  :: x1, x2, x3, x4
	real(8), intent (out) :: x5, x6

	!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	!::: this subroutine computes shape parameters x5 and x6 for the S curve       :::
	!::: equation x = y/(y + exp(x5 + x6*y)) given 2 (x,y) points along the curve. :::
	!::: x5 is determined by solving the equation with x and y values measured     :::
	!::: around the midpoint of the curve (approx. 50% of the maximum value for x) :::
	!::: and x6 is determined by solving the equation with x and y values measured :::
	!::: close to one of the endpoints of the curve (100% of the maximum value for :::
	!::: x) This subroutine is called from readbsn.f and readcrop.f                :::
	!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

	xx = 0.0d0
	x5 = 0.0d0
	x6 = 0.0d0

	xx = Log(x3/x1 - x3)
	x6 = (xx - Log(x4/x2 - x4)) / (x4 - x3)
	x5 = xx + (x3 * x6)

end subroutine Mascrv
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
end module swatwt
