module swatwt_param

implicit none

!----- Parameters -----
real(8) :: snocovmx ! Minimum amount of water for 100% snow cover (mm)
real(8) :: sftmp    ! Snowing temperature (degrees C)
real(8) :: smtmp    ! Melting point (degrees C)
real(8) :: smfmx    ! Maximum melting speed (mm/deg C/day)
real(8) :: smfmn    ! Minimum melting speed (mm/deg C/day)
real(8) :: timp     ! The parameter deciding the lag of effect of temperature 
real(8) :: sol_cov  ! Residues of soil cover (kg/ha)
real(8) :: canmx    ! Maximum water in canopy (mm) (Original value)
real(8) :: gsi      ! Maximum gas conductance 
real(8) :: frgmax   ! Fraction of maximum stomatal conductance at the vapor pressure 
real(8) :: vpdfr    ! Vapor pressure deficit at which FRGMAX is valid 
real(8) :: esco     ! Soil evaporation compensation factor 
real(8) :: esd      ! Maximum soil depth for evaporation (mm) 
real(8) :: etco     ! Rate of water to be evaporating 
real(8) :: hru_dafr ! Fraction of HRU
real(8) :: pot_vol  ! Current volume of water stored in HRU (m^3)
real(8) :: cn2      ! SCS curve number
real(8) :: tlag     ! Lag coefficient for soil temperature 
real(8) :: epco     ! Plant water uptake compensation factor 
real(8) :: ubw      ! Water uptake distribution parameter 
real(8) :: sno50cov ! Shape parameter to determine snow cover rate
real(8) :: mxlai    ! Maximum LAI considering plant density
real(8) :: max_rd   ! Maximum root depth (mm)
integer :: nsol     ! Number of soil layer


!----- Values caluclated later -----
real(8) :: snocov1
real(8) :: snocov2
real(8) :: vpd2


end module swatwt_param
