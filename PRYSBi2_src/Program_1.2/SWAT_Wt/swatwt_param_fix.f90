module swatwt_param_fix

use swatwt_param
use control_param

implicit none

private
public :: s_swatwt_param_fix

contains

subroutine s_swatwt_param_fix

	implicit none
	
	real(8) :: bb1, bb2, bb3, bp1, bp2, bp3
	
	
	!--- Fix number of soil layer ---
	nsol = common_nsol
	
	
	!----- Fixing parameter values -----
	if( target_plant == 'soybean' )      mxlai = 3.0d0
	if( target_plant == 'wheat_winter' ) mxlai = 4.0d0
	if( target_plant == 'maize' .or. target_plant == 'maize_major' .or. target_plant == 'maize_second' ) mxlai = 3.0d0
	if( target_plant == 'rice' .or. target_plant == 'rice_major' .or. target_plant == 'rice_second' ) mxlai = 5.0d0
	if( target_plant == 'wheat_spring' ) mxlai = 4.0d0
	
	if( target_plant == 'soybean' )      max_rd = 1700.0d0
	if( target_plant == 'wheat_winter' ) max_rd = 1300.0d0
	if( target_plant == 'maize' .or. target_plant == 'maize_major' .or. target_plant == 'maize_second' ) max_rd = 2000.0d0
	if( target_plant == 'rice' .or. target_plant == 'rice_major' .or. target_plant == 'rice_second' ) max_rd = 2000.0d0
	if( target_plant == 'wheat_spring' ) max_rd = 2000.0d0

	snocovmx = 1.0d0    ! Minimum amount of water for 100% snow cover (mm)               (Original)
	sftmp    = 1.0d0    ! Snowing temperature (degrees C)                                (SWAT deafult)
	smtmp    = 0.5d0    ! Melting point (degrees C)                                      (SWAT deafult)
	smfmx    = 4.5d0    ! Maximum melting speed (mm/deg C/day)                           (SWAT deafult)
	smfmn    = 4.5d0    ! Minimum melting speed (mm/deg C/day)                           (SWAT deafult)
	timp     = 1.0d0    ! The parameter deciding the lag of effect of temperature        (SWAT default)
	sol_cov  = 500.0d0  ! Residues of soil cover (kg/ha)                                 (Original)
	canmx    = 0.5d0    ! Maximum water in canopy (mm)                                   (Original)
	gsi      = 0.0071d0 ! Maximum gas conductance                                        (SWAT default)
	frgmax   = 0.75d0   ! Fraction of maximum stomatal conductance at the vapor pressure (SWAT default)
	vpdfr    = 4.0d0    ! Vapor pressure deficit at which FRGMAX is valid                (SWAT default)
	esco     = 0.95d0   ! Soil evaporation compensation factor                           (SWAT default)
	esd      = 500.0d0  ! Maximum soil depth for evaporation (mm)                        (SWAT default)
	etco     = 0.8d0    ! Rate of water to be evaporating                                (SWAT default)
	hru_dafr = 1.0d0    ! Fraction of HRU                                                (Because we assume only crop land)
	pot_vol  = 0.0d0    ! Current volume of water stored in HRU (m^3)                    (Because we assume only crop land)
	cn2      = 79.75d0  ! SCS curve number                                               (Average of SWAT default (crops lined straight, good condition))
	tlag     = 0.8d0    ! Lag coefficient for soil temperature                           (SWAT default)
	epco     = 1.0d0    ! Plant water uptake compensation factor                         (SWAT default)
	ubw      = 10.0d0   ! Water uptake distribution parameter                            (SWAT default)
	sno50cov = 0.5d0    ! Shape parameter to determine snow cover rate                   (SWAT default)
	
	
	!----- Calculate parameter -----
	vpd2 = ( 1.0d0 - frgmax ) / ( vpdfr - 1.0d0 )
	
	call Mascrv( 0.5d0, 0.95d0, sno50cov, 0.95d0, snocov1, snocov2 )
	
	
end subroutine s_swatwt_param_fix
!--------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------
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

end module swatwt_param_fix
