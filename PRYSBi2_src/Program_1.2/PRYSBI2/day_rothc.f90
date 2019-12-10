module day_rothc

  use day_rothc_param
  use day_rothc_param_fix
  
  implicit none

  private
  public :: s_day_rothc
  contains

  subroutine s_day_rothc( &
! nparam            , & ! No. of parameters !GS del
  temp              , & ! temperature
! prep              , & ! precipitation !GS del
! pvap              , & ! evaporation !GS del
  pinp              , & ! plant C input
  finp              , & ! manure C input
  covr              , & ! plant cover
  clay              , & ! clay content
  dept              , & ! soil depth
  defc              , & ! water deficit
  soil              , & ! SOC compartments
  CO2g              , & ! released CO2
  totC              , & ! TSOC
  rmtemp            , & ! Soil temperature factor
  d                   & ! Decomposition rate
  )

  !::::: monthly time evolution of SOC	

  implicit none

! integer, intent(in)     :: nparam            ! No. of parameters !GS del
  real(8),  intent(in)    :: temp              ! temperature
! real(8),  intent(in)    :: prep              ! precipitation
! real(8),  intent(in)    :: pvap              ! evaporation
  real(8),  intent(in)    :: pinp              ! plant C input
  real(8),  intent(in)    :: finp              ! manure C input
  integer,  intent(in)    :: covr              ! plant cover
  real(8),  intent(in)    :: clay              ! clay content
  real(8),  intent(in)    :: dept              ! soil depth
  real(8),  intent(inout) :: defc              ! water deficit
  real(8),  intent(inout) :: soil( 5 )         ! SOC compartments
  real(8),  intent(out)   :: CO2g              ! released CO2
  real(8),  intent(out)   :: totC              ! TSOC
  real(8),  intent(out)   :: rmtemp( 4 )       ! Soil temperature factor
  real(8),  intent(out)   :: d( 4 )            ! Decomposition rate

!! Internal
  real(8)   ::  fplant( 4 ), ffarmy( 4 ), Tref
  real(8)   ::  decomp( 4 )
  real(8)   ::  dec( 4 )
! real(8)   ::  Wparam( 4 )  ! params related to water depedency
! real(8)   ::  Lparam( 4 )
! real(8)   ::  Rparam( 4 )
  real(8)   ::  ratem( 4 ), rmsmd( 4 )
  real(8)   ::  drrat, rmcrop
  real(8)   ::  totIN
  real(8)   ::  kc, ee, x, x1, x2, d1, d2, y1, y2
  integer   ::  i
  real(8)   ::  dpm_bio, dpm_hum, rpm_bio, rpm_hum
  real(8)   ::  bio_bio, bio_hum, hum_bio, hum_hum
  real(8)   ::  soilp( 5 ), e( 5 )


  !:::::: DPM/RPM ratio due to plant cover
  drrat = 1.44		 !! crop / improved grassland
!!   drrat = 0.67  !! unimproved grassland / scrub
!!   drrat = 0.25  !! deciduous / tropical woodland

  fplant( 1 ) = drrat / ( 1.0 + drrat )
  fplant( 2 ) = 1.0   / ( 1.0 + drrat )
  fplant( 3 ) = 0.0
  fplant( 4 ) = 0.0


  !:::::: FYM composition (as of rothc)
  ffarmy( 1 ) = 0.49
  ffarmy( 2 ) = 0.49
  ffarmy( 3 ) = 0.00
  ffarmy( 4 ) = 0.02


  !::::: CO2:(BIO+POM) ratio from percent clay value (bpcalc.for)
  x = 1.67 * ( 1.85 + 1.60 * exp( -0.0786 * clay ) )


  !::::: monthly rate modifying factors (ratef.for) :::::::::::: 

  !::::: soil temperature factor
  if( temp.lt.-15.0 ) then  !! modified after King, et al.(1997)
      rmtemp( 1:4 ) = 0.0
  else

!  !::::: Q10 type :::::
!   Tref = 9.3d0
!    do i = 1, 4
!      rmtemp( i ) = Rparam( i ) ** ( ( temp - Tref ) / 10.0d0 )
!    enddo
!
!  !::::: Lloyd & Taylor type :::::
!    Tref = 273.15d0 + 9.3d0
!    T0 = 227.13d0
!    TK = 273.15d0
!    do i = 1, 4
!      rmtemp( i ) = exp( Tparam( i ) * &
!                  ( 1. / (Tref - T0) - 1. / (temp + TK - T0) ) &
!                      )
!    end do

  !::::: as of rothc :::::
    do i = 1, 4
      rmtemp( i ) = Lparam( i ) * 47.9d0 / &
                  ( 1.0 + exp( Rparam( i ) / ( temp + 18.3d0 ) ) )
    end do

  end if


  !::::: soil moisture factor
  do i = 1, 4
     !::::: as of rothc ::::::
		x2 = -1.0 * ( 20.0 + 1.3 * clay - 0.01 * ( clay * clay ) )
!   x2 = -Wparam( i ) * ( 20.0 + 1.3 * clay - 0.01 * ( clay * clay ) )
    x2 = x2 * dept / 23.0   !! max. SMD
    d1 = x2 / 1.8           !! for bare soil (d1<d2)
    d2 = x2                 !! for vegetated
		x2 = Wparam(i) * x2
    x1 = 0.444 * x2         !! x2<x1
    y1 = 1.0                !! x2=d2<d1<x1<0
    y2 = 0.2

    kc = 0.75
!    ee = pvap * kc  !! convert to crop evapotranspiration !GS del

!    if( prep.gt.ee ) then
!      defc = defc + ( prep - ee ) !GS del
!      goto 90
!    end if

    if( covr.eq.1 ) goto 80

!    defc = defc + ( prep - ee ) !GS del
    if( defc.lt.d1 ) defc = d1

    goto 90

80  continue
!    defc = defc + ( prep - ee ) !GS del
    if( defc.lt.d2 ) defc = d2

90  continue
    if( defc.gt.0.0 ) defc = 0.0

!100 continue !GS del

    if( defc.ge.x1 ) then
      rmsmd( i ) = 1.0
      goto 110
    end if

    if( defc.ge.x2 ) then
      rmsmd( i ) = y2 + ( y1 - y2 ) * ( ( x2 -defc ) / ( x2 - x1 ) )
		else
			rmsmd( i ) = y2
    end if

110 continue
  end do

  !::::: crop retaining factor
  if( covr.eq.1 ) then
    rmcrop = 0.6
  else
    rmcrop = 1.0
  endif

  !::::: combining rate modifiers
  ratem( 1:4 ) = rmtemp( 1:4 ) * rmsmd( 1:4 ) * rmcrop

  !::::: decomposition rate
    !::::: intrinsic decomposition rate
    decomp( 1 ) = 10.0   !! DPM
    decomp( 2 ) = 0.30   !! RPM
    decomp( 3 ) = 0.66   !! BIO
    decomp( 4 ) = 0.02   !! HUM
!	decomp( 4 ) = decomp( 4 ) / ( 1.262 + 2.27 * Alp) !! Andosols

!	dec( 1:4 ) = decomp( 1:4 ) / 12.0
	dec( 1:4 ) = decomp( 1:4 ) / 365.25d0 !GS
  
  
  d( 1:4 ) = dec( 1:4 ) * ratem( 1:4 )  !! environmetal factor
  
  do i = 1, 4
    if( d( i ).gt.1.0e-7 ) then
      e( i ) = exp( -d( i ) )
    else
      e( i ) = 1.0
    end if
  end do

  !::::: transfer matrix (i->j)
  dpm_bio = 0.46 / ( 1.0 + x )
  dpm_hum = 0.54 / ( 1.0 + x )
  rpm_bio = 0.46 / ( 1.0 + x )
  rpm_hum = 0.54 / ( 1.0 + x )
  bio_bio = 0.46 / ( 1.0 + x )
  bio_hum = 0.54 / ( 1.0 + x )
  hum_bio = 0.46 / ( 1.0 + x )
  hum_hum = 0.54 / ( 1.0 + x )


  !::::: Organic matter input
  soil( 1:4 ) = soil( 1:4 ) + fplant( 1:4 ) * pinp + ffarmy( 1:4 ) * finp

  totC = 0.0

  totC = sum( soil( 1:5 ) )

  totIN = totC

  soilp( 1 ) = soil( 1 ) * e( 1 )     ! DPM
  soilp( 2 ) = soil( 2 ) * e( 2 )     ! RPM
  soilp( 3 ) = soil( 3 ) * e( 3 )  &  ! BIO
             + soil( 1 ) * dpm_bio * ( 1.0 - e( 1 ) ) &
             + soil( 2 ) * rpm_bio * ( 1.0 - e( 2 ) ) &
             + soil( 3 ) * bio_bio * ( 1.0 - e( 3 ) ) &
             + soil( 4 ) * hum_bio * ( 1.0 - e( 4 ) ) 
  soilp( 4 ) = soil( 4 ) * e( 4 )  &  ! HUM
             + soil( 1 ) * dpm_hum * ( 1.0 - e( 1 ) ) &
             + soil( 2 ) * rpm_hum * ( 1.0 - e( 2 ) ) &
             + soil( 3 ) * bio_hum * ( 1.0 - e( 3 ) ) &
             + soil( 4 ) * hum_hum * ( 1.0 - e( 4 ) ) 
  soilp( 5 ) = soil( 5 )              ! IOM

  soil( 1:5 ) = soilp( 1:5 )

  totC = 0.0
  do i = 1, 5
    totC = totC + soil( i )
  end do
  CO2g = ( totIN - totC )  !! released CO2 [tC/ha]

  end subroutine s_day_rothc


end module day_rothc
