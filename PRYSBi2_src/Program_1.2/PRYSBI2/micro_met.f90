module micro_met

implicit none

private
public :: s_micro_met
contains

! This subroutine calculate Wind speed at the canopy top and Aerodynamic condactance
! according to Neitsch et al. 2005. (SWAT model)

subroutine s_micro_met( &
	cht,     & !in  Canopy height                       (m)
	ws,      & !in  Wind speed at uzz                   (m/s)
	uzz,     & !in  Height at which wind speed measured (m)
	ws_top,  & !out Wind speed at the canopy top        (m/s)
	ga)        !out Aerodynamic conductance             (m/s)
	
	implicit none
	
	real(8), intent(in)  :: cht
	real(8), intent(in)  :: ws
	real(8), intent(in)  :: uzz
	real(8), intent(out) :: ws_top
	real(8), intent(out) :: ga
	
	real(8) :: chz, zom, zov, d, ra

    !----- Convert the unit of canopy height (from m to cm) -----
    if ( cht < 0.01d0 ) then
    chz = 1.0d0
    else
    chz = cht * 100.0d0 ! from m to cm
    endif

    !----- calculate roughness length for momentum transfer (cm) -----
    if( chz <= 200.0d0 ) then
    zom = 0.123d0 * chz
    else
    zom = 0.058d0 * chz**1.19d0
    endif

    !----- calculate roughness length for vapor transfer (cm) -----
    zov = 0.1d0 * zom

    !----- calculate zero-plane displacement of wind profile (cm) -----
    d = 0.667d0 * chz

    !----- calculate aerodynamic resistance -----
    ra = log( ( 100.0d0*uzz - d ) / zom ) * log( ( 100.0d0*uzz - d ) / zov )
    ra = ra / ( ( 0.41d0 )**2 * (ws+0.1d0) )
	ga = 1.0d0/ra
    ga = max(0.01d0, ga)

    !---- calculate wind speed -----
	ws_top = ws * (cht/uzz)**0.2d0

end subroutine s_micro_met

end module micro_met
