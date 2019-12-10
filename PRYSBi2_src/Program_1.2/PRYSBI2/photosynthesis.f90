module photosynthesis

use spm_param

implicit none

private
public :: s_photosynthesis_big_leaf
contains

! In this subroutine, the daily assimilation rate (kg C/ha/day) is calculated.
! This subroutine calculate net assimilation rate using Farquhar's model.
! We reffered Sellers et al. 1996a for the exposition of Farquhar's model C3 and Collatz model for C4 plants.

subroutine s_photosynthesis_big_leaf( &
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
as,              & !out Net assimilation rate                                                           (µmol C/m2/day)
cmass)             !out Net assimilation rate                                                           (kg C/ha/day)

implicit none

real(8), intent(in)  :: strs_fac
integer, intent(in)  :: doy
real(8), intent(in)  :: lati
real(8), intent(in)  :: sr
real(8), intent(in)  :: tx
real(8), intent(in)  :: tn
real(8), intent(in)  :: tn_next
real(8), intent(in)  :: rh
real(8), intent(in)  :: ca
real(8), intent(in)  :: oa
real(8), intent(in)  :: lai
real(8), intent(in)  :: press
real(8), intent(in)  :: ga
real(8), intent(in)  :: ws_top
real(8), intent(out) :: as
real(8), intent(out) :: cmass

!--- Additional variable declaration ---
real(8), parameter :: cq     = 4.56d0 ! Conversion factor for solar radiation (Yin & van Laar 2005)
real(8), parameter :: cnvpfd = 0.473d0 ! PFD to PPFD (Papaioannou et al. 1993 Table 1)

integer :: i
real(8) :: gx5(5), gw5(5), tm, srj, rh_rate
real(8) :: daylength, tgt_pfd, tgt_ppfd
real(8) :: as_tmp(5), xxx(5), vcmax25, jmax25, rd25
real(8) :: ci_ca, gb, gab, gs, tgt_time, tgt_srj, tgt_tm, uu, gamol, gbmol, gabmol

data gx5/0.04691d0, 0.23075d0, 0.50000d0, 0.76925d0, 0.95309d0/ ! (Yin & van Laar 2005)
data gw5/0.11846d0, 0.23931d0, 0.28444d0, 0.23931d0, 0.11846d0/ ! (Yin & van Laar 2005)

!--- Zero clear ---
as    = 0.0d0
cmass = 0.0d0
gs    = 0.0d0
ci_ca = 0.0d0

!--- Calc tm ---
tm = (tx + tn) / 2.0d0

!--- Convert ---
srj = sr * 1.0d6 ! From (MJ/m2/day) to (J/m2/day)

!--- Conversion ---
rh_rate  = rh / 100d0 ! From (%) to (rate)

!--- Calculate day length -----
call dla(dble(doy), lati, daylength)

do i = 1, 5 ! roop #00

    !--- Set target time ---
    tgt_time = (12.0d0 - daylength/2.0d0) + daylength*gx5(i) ! (Yin & van Laar 2005)


    !--- Calculate air temperature and radiation at each time ---
    call tm_cource(tgt_time, daylength, tx, tn_next, tgt_tm)
    call par_cource(tgt_time, dble(doy), lati, srj, daylength, tgt_srj)

    !--- Calculate Laminar boundary layer conductance ---
    uu    = ws_top
    gb    = 0.01d0*sqrt(uu/leaf_w)*(1.0d0-exp(-0.5d0*kw* (lai + 0.01d0)))/(0.5d0*kw) ! (m/s) ! (Yin &van Laar 2005. p16 for heat)
    gb    = max(0.0001d0, gb)
    gbmol = gb * press / 8.314462d0 / (273.16d0 + tgt_tm) ! From (m/s) to (mol/m2/s)
    gbmol = gbmol / 2.8d0 ! From sensible heat to CO2 (Sellers et al. 1996a. p.683)

    gamol = ga * press / 8.314462d0 / (273.16d0 + tgt_tm) ! From (m/s) to (mol/m2/s)

    gabmol = 1d0 / (1d0/gamol + 1d0/gbmol) ! aerodynamic conductance + boundary layer conductance

    !--- Calculate PPFD absorved ---
    tgt_pfd  = tgt_srj * cq ! from (J/m2/s) to (µmol/m2/s)
    tgt_ppfd = cnvpfd * tgt_pfd * ( 1.0d0 - exp( -ms_ext_coef * ( lai + 0.01d0 ) ) ) ! (Neitsch et al. 2005) (Monsi-Saeki equation)
    tgt_ppfd = tgt_ppfd / (lai + 0.01d0) ! PPFD per unit leaf area (m2)

    !--- Calculate Vcmax, Jmax, RD
    vcmax25 = vtop
    jmax25  = 1.67d0 * vcmax25 ! (Medlyn et al. 2002 PCE. p1175)
    rd25    = vcmax25*rd25_fac ! (Sellers et al. 1996a. p.699) (rd25_fac=0.015 for C3, 0.025 for C4)

    !--- Adjustment of Vcmax and Jmax if needed ---
    vcmax25 = adjust_vcmax25 * vcmax25 * strs_fac
    jmax25  = adjust_jmax25 * jmax25 * strs_fac
    rd25    = adjust_rd25_fac * rd25

    if(path == 1) then
        call assimilation_c3( &
            tgt_ppfd, & !in  PAR                                                      (µmol/m2/s)
            tgt_tm,   & !in  Temperature                                              (deg C)
            rh_rate,  & !in  Relative humidity                                        (rate)
            ca,       & !in  Co2 consentration of atmosphere                          (ppm)
            oa,       & !in  O2 consentration of atmosphere                           (ppm)
            press,    & !in  Atomosphere pressure                                     (Pa)
            b_dash,   & !in  Parameter1 of Ball model                                 (mol CO2/m2/s)
            mm,       & !in  Parameter2 of Ball model                                 (-)
            gabmol,   & !in  Laminar boundary layer conductance                       (mol CO2/m2/s)

            phi,      & !in  Intrinsic quantum efficiency                             (mol/mol)
            kmc25,    & !in  Michaelis-Menten constant for CO2 at 25 deg C            (µ mol/mol)
            kmo25,    & !in  Michaelis-Menten constant for O2 at 25 deg C             (µ mol/mol)
            ekmc,     & !in  Activation energy for kmc                                (J/mol)
            ekmo,     & !in  Activation energy for kmo                                (J/mol)

            vcmax25,  & !in  Vcmax at 25 deg C                                        (µmol/m2/s)
            evcmax,   & !in  Acviation energy for Vcmax                               (J/mol)
            dvcmax,   & !in  Deactivation energy for Vcmax                            (J/mol)
            svcmax,   & !in  Entropy term for Vcmax                                   (J/K/mol)

            rd25,     & !in  Respiration rate at 25 deg C                             (µmol/m2/s)
            erd,      & !in  Acviation energy for Rd                                  (J/mol)
            jmax25,   & !in  Jmax at 25 deg C                                         (µmol/m2/s)
            ejmax,    & !in  Acviation energy for J                                   (J/mol)
            djmax,    & !in  Deactivation energy for J                                (J/mol)
            sjmax,    & !in  Entropy term for J                                       (J/K/mol)

            cup_cfj,  & !in  Coupling coefficient for J                               (-)

            as_tmp(i),& !out Assimilation rate										 (µmol C/m2/s)
            gs,       & !out Stomatal conductance                                     (mol CO2/m2/s) !new
            ci_ca     & !out The ratio of Ci to Ca                                    (rate)
            )
    endif

    if(path == 2) then
        call assimilation_c4( &
            tgt_ppfd, & !in  PPFD                                                     (µmol/m2/s)
            tgt_tm,   & !in  Temperature                                              (deg C)
            rh_rate,  & !in  Relative humidity                                        (rate)
            ca,       & !in  Co2 consentration of atmosphere                          (ppm)
            press,    & !in  Atomosphere pressure                                     (Pa)
            b_dash,   & !in  Parameter1 of Ball model                                 (mol CO2/m2/s)
            mm,       & !in  Parameter2 of Ball model                                 (-)
            gabmol,   & !in  Laminar boundary layer conductance                       (mol CO2/m2/s)

            phi,      & !in  Intrinsic quantum efficiency                             (mol/mol)

            vcmax25,  & !in  Vcmax at 25 deg C                                        (µmol/m2/s)
            evcmax,   & !in  Acviation energy for Vcmax                               (J/mol)
            dvcmax,   & !in  Deactivation energy for Vcmax                            (J/mol)
            svcmax,   & !in  Entropy term for Vcmax                                   (J/K/mol)

            rd25,     & !in  Respiration rate at 25 deg C                             (µmol/m2/s)
            erd,      & !in  Acviation energy for Rd                                  (J/mol)

            jmax25,   & !in  Jmax at 25 deg C                                         (µmol/m2/s)
            ejmax,    & !in  Acviation energy for J                                   (J/mol)
            djmax,    & !in  Deactivation energy for J                                (J/mol)
            sjmax,    & !in  Entropy term for J                                       (J/K/mol)

            cup_cfj,  & !in  Coupling coefficient for J                               (-)
            kapa,     & !in  Parameter for C4 photosynthesis model                    (-)
            as_tmp(i),& !out Assimilation rate                                        (µmol C/m2/s)
            gs,       & !out Stomatal conductance                                     (mol CO2/m2/s) !new
            ci_ca     & !out The ratio of Ci to Ca                                    (rate)
            )
    endif

enddo ! roop #00

!--- Weighted averaging ---
xxx(:) = 0.0d0
do i = 1, 5
xxx(i) = 3600.0d0 * daylength * (as_tmp(i) * lai) * gw5(i)  ! From (µmol/m2/s) to (µmol/m2/day)
enddo
as = sum(xxx(:))

cmass = as * 1.0d-6                     ! From (µmol/m2/day) to (mol/m2/day)
cmass = cmass * 12.0d0 * 1.0d-3 * 1.0d4 ! From (mol/m2/day) to (kg C/ha/day)

end subroutine s_photosynthesis_big_leaf
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine assimilation_c3(   &
					ppfd,    & !in  PPFD                                                     (µmol/m2/s)
					tmp,     & !in  Temperature                                              (deg C)
					rh,      & !in  Relative humidity                                        (rate)
					caa,     & !in  Co2 consentration of atmosphere                          (ppm)
					oaa,     & !in  O2 consentration of atmosphere                           (ppm)
					press,   & !in  Atomosphere pressure                                     (Pa)
					b_dash,  & !in  Parameter1 of Ball model                                 (mol CO2/m2/s)
					mm,      & !in  Parameter2 of Ball model                                 (-)
					gb,      & !in  Laminar boundary layer conductance                       (mol CO2/m2/s)
					phi,     & !in  Intrinsic quantum efficiency                             (mol/mol)

					kmc25,   & !in  Michaelis-Menten constant for CO2 at 25 deg C            (µ mol/mol)
					kmo25,   & !in  Michaelis-Menten constant for O2 at 25 deg C             (µ mol/mol)
					ekmc,    & !in  Activation energy for kmc                                (J/mol)
					ekmo,    & !in  Activation energy for kmo                                (J/mol)

					vcmax25, & !in  Vcmax at 25 deg C                                        (µmol/m2/s)
					evcmax,  & !in  Acviation energy for Vcmax                               (J/mol)
					dvcmax,  & !in  Deactivation energy for Vcmax                            (J/mol)
					svcmax,  & !in  Entropy term for Vcmax                                   (J/K/mol)

					rd25,    & !in  Respiration rate at 25 deg C                             (µmol/m2/s)
					erd,     & !in  Acviation energy for Rd                                  (J/mol)

					jmax25,  & !in  Jmax at 25 deg C                                         (µmol/m2/s)
					ejmax,   & !in  Acviation energy for J                                   (J/mol)
					djmax,   & !in  Deactivation energy for J                                (J/mol)
					sjmax,   & !in  Entropy term for J                                       (J/K/mol)

					cup_cfj, & !in  Coupling coefficient for J                               (-)

					an,      & !out Assimilation rate                                        (µmol C/m2/s)
					gs,      & !out Stomatal conductance                                     (mol CO2/m2/s)
					ci_ca    & !out The ratio of Ci to Ca                                    (rate)
					)

	! This subroutine calculates Assimilation rate (µmol/m2/s) of C3 crop.
	! This subroutine mainly refers to Sellers et al. 1996

	! (Reference)
	! Sellers et al. A revised land surface parameterization (SiB2) for atmospheric GCMs Part I: Model Formulation

	implicit none
	
	real(8), intent(in)  :: ppfd
	real(8), intent(in)  :: tmp
	real(8), intent(in)  :: rh
	real(8), intent(in)  :: caa
	real(8), intent(in)  :: oaa
	real(8), intent(in)  :: press
	real(8), intent(in)  :: b_dash
	real(8), intent(in)  :: mm
	real(8), intent(in)  :: gb
	real(8), intent(in)  :: phi
	real(8), intent(in)  :: kmc25
	real(8), intent(in)  :: kmo25
	real(8), intent(in)  :: ekmc
	real(8), intent(in)  :: ekmo
	real(8), intent(in)  :: vcmax25
	real(8), intent(in)  :: evcmax
	real(8), intent(in)  :: dvcmax
	real(8), intent(in)  :: svcmax
	real(8), intent(in)  :: jmax25
	real(8), intent(in)  :: rd25
	real(8), intent(in)  :: erd
	real(8), intent(in)  :: ejmax
	real(8), intent(in)  :: djmax
	real(8), intent(in)  :: sjmax
	real(8), intent(in)  :: cup_cfj
	real(8), intent(out) :: an
	real(8), intent(out) :: gs
	real(8), intent(out) :: ci_ca

	real(8) :: kc, ko, co2_comp
	real(8) :: a1, a2, b1, b2, d1, d2, e1, e2
	real(8) :: aa, bb, dd, ee
	real(8) :: oo, pp, qq, rr
	real(8) :: ac, aj, as, cs, ci
	real(8) :: vcmax, jmax, alpha_q, jj, rd, ca, oa
    real(8) :: tmp_an, diff
    real(8) :: alpha, beta, gamma, theta

	! Zero clear
	kc       = 0d0
	ko       = 0d0
	co2_comp = 0d0
	a1       = 0d0
	a2       = 0d0
	b1       = 0d0
	b2       = 0d0
	d1       = 0d0
	d2       = 0d0
	e1       = 0d0
	e2       = 0d0
	aa       = 0d0
	bb       = 0d0
	dd       = 0d0
	ee       = 0d0
	oo       = 0d0
	pp       = 0d0
	qq       = 0d0
	rr       = 0d0
	ac       = 0d0
	aj       = 0d0
	cs       = 0d0
	ci       = 0d0
	vcmax    = 0d0
	jmax     = 0d0
	jj       = 0d0
	alpha_q  = 0d0
	rd       = 0d0

	!----- Correction -----
	ca = caa * press / 101325.0d0
	oa = oaa * press / 101325.0d0


	call arrhenius_res(kmc25, ekmc, tmp, kc)
	call arrhenius_res(kmo25, ekmo, tmp, ko)
	call arrhenius_res(rd25,  erd,  tmp, rd)
	call arrhenius_res(vcmax25, evcmax * adjust_e, tmp, vcmax)
	call peaked_res(jmax25, ejmax * adjust_e, sjmax, djmax, tmp, jmax)

	alpha_q  = phi * ppfd ! Here, phi should be intrinsic quantum efficiency used in Sellers et al. 1996a
	jj       = ( jmax + alpha_q - sqrt( (jmax + alpha_q)**2 - 4.0d0*cup_cfj*jmax*alpha_q ) )/(2.0d0*cup_cfj)
	co2_comp = 0.5d0 * oa * (kc/ko) * (0.21d0*vcmax/vcmax)

	!----- Calculation -----
    ! Calc from alpha to theta
    alpha = 1.0d0 + b_dash/gb - mm*rh
    beta  = ca*(gb*mm*rh - 2.0d0*b_dash - gb)
    gamma = (ca**2)*b_dash*gb
    theta = gb*mm*rh - b_dash

	! Set from a to e
	a1 = vcmax
	b1 = kc*(1.0d0 + oa/ko)
	d1 = co2_comp
	e1 = 1.0d0

	a2 = jj
	b2 = 2.0d0*co2_comp ! b2 is not 8.0d0*co2_comp because we use intrinsic quantum efficiency used in Sellers et al. 1996a
	d2 = co2_comp
	e2 = 1.0d0! e2 is not 4.0d0 because we use intrinsic quantum efficiency used in Sellers et al. 1996a

    ! Calc Ac
    aa = a1; bb = b1; dd = d1; ee = e1
    oo = ee*alpha
    pp = ee*beta+bb*theta-aa*alpha+ee*alpha*rd
    qq = ee*gamma+bb*gamma/ca-aa*beta+aa*dd*theta+ee*rd*beta+rd*bb*theta
    rr = -aa*gamma+aa*dd*gamma/ca+ee*rd*gamma+rd*bb*gamma/ca
    pp = pp/oo
    qq = qq/oo
    rr = rr/oo
    call qubic_solve(pp,qq,rr, ac)

    ! Calc Aj
    aa = a2; bb = b2; dd = d2; ee = e2
    oo = ee*alpha
    pp = ee*beta+bb*theta-aa*alpha+ee*alpha*rd
    qq = ee*gamma+bb*gamma/ca-aa*beta+aa*dd*theta+ee*rd*beta+rd*bb*theta
    rr = -aa*gamma+aa*dd*gamma/ca+ee*rd*gamma+rd*bb*gamma/ca
    pp = pp/oo
    qq = qq/oo
    rr = rr/oo
    call qubic_solve(pp,qq,rr, aj)

    ! Calc As
    as = vcmax / 2d0 - rd ! (Sellers et al. 1996a)

    ! Calc an
	an = min(ac, aj, as)

	cs = ca - an/gb
	gs = mm*an*rh/cs + b_dash
	ci = cs - an/gs
	ci_ca = ci/ca

end subroutine assimilation_c3
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine assimilation_c4( &
	par,     & !in  PAR                                                      (µmol/m2/s)
	tmp,     & !in  Temperature                                              (deg C)
	rh,      & !in  Relative humidity                                        (rate)
	caa,     & !in  Co2 consentration of atmosphere                          (ppm)
	press,   & !in  Atomosphere pressure                                     (Pa)
	b_dash,  & !in  Parameter1 of Ball model                                 (mol CO2/m2/s)
	mm,      & !in  Parameter2 of Ball model                                 (-)
	gb,      & !in  Laminar boundary layer conductance                       (mol CO2/m2/s)

	phi,     & !in  Intrinsic quantum efficiency                             (mol/mol)

	vcmax25, & !in  Vcmax at 25 deg C                                        (µmol/m2/s)
	evcmax,  & !in  Acviation energy for Vcmax                               (J/mol)
	dvcmax,  & !in  Deactivation energy for Vcmax                            (J/mol)
	svcmax,  & !in  Entropy term for Vcmax                                   (J/K/mol)

	rd25,    & !in  Respiration rate at 25 deg C                             (µmol/m2/s)
	erd,     & !in  Acviation energy for Rd                                  (J/mol)

	jmax25,  & !in  Jmax at 25 deg C                                         (µmol/m2/s)
	ejmax,   & !in  Acviation energy for J                                   (J/mol)
	djmax,   & !in  Deactivation energy for J                                (J/mol)
	sjmax,   & !in  Entropy term for J                                       (J/K/mol)

	cup_cfj, & !in  Coupling coefficient for J                               (-)
	kapa,    & !in  Parameter for C4 photosynthesis model                    (-)
	an,      & !out Assimilation rate                                        (µmol C/m2/s)
	gs,      & !out Stomatal conductance                                     (mol CO2/m2/s)
	ci_ca    & !out The ratio of Ci to Ca                                    (rate)
	)


    ! This subroutine calculates Assimilation rate (µmol/m2/s) of C3 crop.
    ! This subroutine mainly refers to Sellers et al. 1996
    ! For cs, the equation is our original. From Ball equation and Farqhar equation, we derived the
    ! analytical solution of C4 assimilation rate.

    ! (Reference)
    ! Sellers et al. A revised land surface parameterization (SiB2) for atmospheric GCMs Part I: Model Formulation

	! Collatz, G. J., Ribas-Carbo, M. & Berry, J. A. (1992) Coupled photosynthesis-stomatal conductance model
	! for leaves of C4 plants. Aust. J. Plant Physiol., 19, 519-538.

	! Baldocchi D (1994) An analytical solution for coupled leaf photosynthesis and stomatal conductance models.
	! Tree Physiology 14:1069-1079.

	implicit none

	real(8), intent(in)  :: par
	real(8), intent(in)  :: tmp
	real(8), intent(in)  :: rh
	real(8), intent(in)  :: caa
	real(8), intent(in)  :: press
	real(8), intent(in)  :: b_dash
	real(8), intent(in)  :: mm
	real(8), intent(in)  :: gb
	real(8), intent(in)  :: phi
	real(8), intent(in)  :: vcmax25
	real(8), intent(in)  :: evcmax
	real(8), intent(in)  :: dvcmax
	real(8), intent(in)  :: svcmax
	real(8), intent(in)  :: jmax25
	real(8), intent(in)  :: rd25
	real(8), intent(in)  :: erd
	real(8), intent(in)  :: ejmax
	real(8), intent(in)  :: djmax
	real(8), intent(in)  :: sjmax
	real(8), intent(in)  :: cup_cfj
	real(8), intent(in)  :: kapa
	real(8), intent(out) :: an
	real(8), intent(out) :: gs
	real(8), intent(out) :: ci_ca

	real(8) :: ac, aj, as, cs, ci

	real(8) :: vcmax, jmax, alpha_q, jj, rd, ca, oa

	! Zero clear
	ac       = 0d0
	aj       = 0d0
	as       = 0d0
	cs       = 0d0
	ci       = 0d0
	vcmax    = 0d0
	jmax     = 0d0
	jj       = 0d0
	alpha_q  = 0d0
	rd       = 0d0

	!----- Correction -----
	ca = caa * press / 101325.0d0

	call arrhenius_res(rd25, erd, tmp, rd)
	call arrhenius_res(vcmax25, evcmax * adjust_e, tmp, vcmax)
	call peaked_res(jmax25, ejmax * adjust_e, sjmax, djmax, tmp, jmax)

	alpha_q  = phi * par ! Here, phi should be intrinsic quantum efficiency used in Sellers et al. 1996.
	jj       = ( jmax + alpha_q - sqrt( (jmax + alpha_q)**2 - 4.0d0*cup_cfj*jmax*alpha_q ) )/(2.0d0*cup_cfj)


	!--- calc Ac ---
	ac = vcmax - rd ! (Sellers et al. 1996a)

  !--- calc Aj ---
  aj = jj - rd ! (Sellers et al. 1996a)

  !--- Calc As ---
!  as = (gb*sqrt(gb**2*mm**2*Rh**4+(-2.0d0*ca*gb**2*kapa*mm**2*vcmax-2.0d0*b_dash*gb*mm)*Rh**3 &
!  & +(ca**2*gb**2*kapa**2*mm**2*vcmax**2+(2.0d0*ca*gb**2+2.0d0*b_dash*ca*gb)*kapa*mm*vcmax- &
!  & 2.0d0*b_dash*ca*gb**2*mm+b_dash**2)*Rh**2+(-2.0d0*ca**2*gb**2*kapa**2*mm*vcmax**2+ &
!  & (2.0d0*b_dash*ca**2*gb**2*kapa*mm+2.0d0*b_dash*ca*gb*kapa)*vcmax+2.0d0*b_dash**2*ca*gb)*Rh+ &
!  & ca**2*gb**2*kapa**2*vcmax**2+2.0d0*b_dash*ca**2*gb**2*kapa*vcmax+b_dash**2*ca**2*gb**2)- &
!  & gb**2*mm*Rh**2+(ca*gb**2*kapa*mm*vcmax+b_dash*gb)*Rh+(-ca*gb**2-2.0d0*b_dash*ca*gb)*kapa*vcmax- &
!  & b_dash*ca*gb**2)/((2.0d0*gb*kapa*mm*vcmax+2.0d0*gb**2*mm)*Rh+ &
!  & (-2.0d0*gb-2.0d0*b_dash)*kapa*vcmax-2.0d0*b_dash*gb) - rd ! (Sellers et al. 1996a)

	!--- calc An ---
!	an = min(as, aj, ac)
  an = min(aj, ac)

	cs = ca - an/gb
	gs = mm*an*rh/cs + b_dash
	ci = cs - an/gs
	ci_ca = ci/ca

	!--- If ci:ca ratio is larger than optimal ratio of C4 ...---
	if( ci_ca > 0.4d0 ) then

        as = vcmax - rd

        !--- recalc An ---
        an = min(ac, aj, as)

        !--- recalc ci:ca ---
        cs = ca - an/gb
        gs = mm*an*rh/cs + b_dash
        ci = cs - an/gs
        ci_ca = ci/ca

	endif

end subroutine assimilation_c4
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine qubic_solve(qs_p, qs_q, qs_r, qs_result)

	implicit none

	real(8), parameter :: qs_pi = 2.0d0*acos(0.0d0)
	real(8) :: qs_p, qs_q, qs_r, qs_result
	real(8) :: qs_qq, qs_rr, qs_ss, qs_theta

	! This subroutine calculate the solution of 3th degree equation.

	qs_qq = (qs_p*qs_p - 3.0d0*qs_q)/9.0d0
	qs_qq = abs(qs_qq)
	qs_rr = (2.0d0*qs_p*qs_p*qs_p - 9.0d0*qs_p*qs_q + 27.0d0*qs_r)/54.0d0
	qs_ss = ( qs_rr/sqrt(qs_qq**3) )

	if( qs_ss < -1.0d0 ) qs_ss = -1.0d0
	if( qs_ss > 1.0d0  ) qs_ss = 1.0d0

	qs_theta = acos(qs_ss)

	qs_result = -2.0d0*sqrt(qs_qq)*cos( (qs_theta + 4.0d0*qs_pi)/3.0d0 ) - qs_p/3.0d0

end subroutine qubic_solve
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine arrhenius_res(p25, ep, tmp, arr_result)

	! Reference
	! Kaschuk et al. (2012) Environmental and Experimental Botany 76, 1-6.
	
	implicit none
	
	real(8) :: p25, ep, tmp, arr_result
	
	arr_result = p25 * exp( ep*(tmp - 25.0d0)/(298.0d0*8.314d0*(273.0d0 + tmp)) )
	
end subroutine arrhenius_res
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine peaked_res(p25, ep, sp, dp, tmp, peaked_result)

	! Reference
	! Medlyn et al. (2002) Plant, Cell and Environment, 25, 1167-1179.
	
	implicit none
	
	real(8) :: p25, ep, sp, dp, tmp, peaked_result
	
	peaked_result = p25*exp( (ep*(tmp-25.0d0))/(298.0d0*8.314d0*(273.0d0+tmp)) ) * &
	            & (1+exp( (298.0d0*sp-dp)/(298.0d0*8.314d0) ) ) / &
	            & (1+exp( ((tmp+273.0d0)*sp-dp) /(8.314d0*(tmp+273.0d0)) ) )

end subroutine peaked_res
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine par_cource(tgt_time, doy, lati, par, dla_tmp, tgt_par)

    implicit none

    ! This subroutine calculate instantaneous radiation
    double precision ::tgt_time, doy, lati, par, dla_tmp, tgt_par
    double precision :: delta, a, b, sin_b, dsinb
    double precision, parameter :: pi = 3.14159265

    delta = - asin( sin(23.45d0*pi/180.0d0) * cos(2.0d0*pi*(doy + 10.0d0) / 365.0d0) )
    a     = sin(pi*lati/180.0d0)*sin(delta)
    b     = cos(pi*lati/180.0d0)*cos(delta)
    sin_b = a + b * cos(2.0d0 * pi * (tgt_time - 12.0d0) / 24.0d0)
    dsinb = 3600.0d0*(dla_tmp*a + (24.0d0/pi) * b * sqrt( 1.0d0 - (a/b)**2))
    tgt_par = max(0.0d0, par * sin_b * (1.0d0 + 0.033d0*cos(2.0d0*pi*(doy-10.0d0)/365.0d0))/dsinb)

end subroutine par_cource
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine dla( &
    doy,      & ! DOY                                (day)
    lati,     & ! Latitude                           (deg)
    daylength & ! Daylength (from sunrise to sunset) (hour)
    )

    ! This subroutine calculate daylength from DOY and latitude.
    ! Yin and van Laar (2005) Crop Systems Dynamics: An Ecophysiological
    ! Simulation Model for Genotype-by-environment, Wageningen Pers. p. 73.

    implicit none

    real(8) :: lati, daylength, doy
    real(8) :: delta, a, b, xxx
    real(8), parameter :: pi = 3.14159265d0

    delta = -asin( sin(23.45d0*pi/180.0d0) * cos(2.0d0*pi*(doy + 10.0d0) / 365.0d0) )
    a     = sin(pi*lati/180.0d0)*sin(delta)
    b     = cos(pi*lati/180.0d0)*cos(delta)
    xxx   = a/b

    if( xxx > 1 ) then
    daylength = 24.0d0
    elseif( xxx < -1 ) then
    daylength = 0.0d0
    else
    daylength   = 12.0d0*( 1.0d0+(2.0d0/pi) * asin(xxx) )
    endif

end subroutine dla
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine dlp( &
    doy,        & ! DOY                                     (day)
    lati,       & ! Latitude                                (deg)
    alpha,      & ! Minimum solar elevation during twilight (deg)
    p_daylength & ! Photoperiodic daylength                 (hour)
    )

    ! This subroutine calculate photoperiodic daylength from DOY and latitude.
    ! Yin and van Laar (2005) Crop Systems Dynamics: An Ecophysiological
    ! Simulation Model for Genotype-by-environment, Wageningen Pers. p. 73.

    implicit none

    real(8) :: lati, p_daylength, doy, alpha
    real(8) :: delta, a, b, xxx
    real(8), parameter :: pi = 3.14159265d0

    delta = -asin( sin(23.45d0*pi/180.0d0) * cos(2.0d0*pi*(doy + 10.0d0) / 365.0d0) )
    a     = sin(pi*lati/180.0d0)*sin(delta)
    b     = cos(pi*lati/180.0d0)*cos(delta)
    xxx   = (-sin(alpha*pi/180.0d0)+a)/b

    if( xxx > 1 ) then
    p_daylength = 24.0d0
    elseif( xxx < -1 ) then
    p_daylength = 0.0d0
    else
    p_daylength   = 12.0d0*( 1.0d0+(2.0d0/pi) * asin( xxx ) )
    endif

end subroutine dlp
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine tm_cource( &
    tgt_time,  & !in  Target time                    (hour)
    daylength, & !in  Day length                     (hout)
    tx,        & !in  Maximum temperature            (deg C)
    tn,        & !in  Minimum temperature            (deg C)
    tgt_tm     & !out Air temperature at target time (deg C)
    )

    ! This subroutine calculate air temperature at target time from
    ! maximum temperature, minimum temperature, and day length

    ! Reference
    ! Thornley, J. H. M. & France, J. (2004) Mathematical Models in Agriculture:
    ! Quantitative Methods for the Plant, Animal and Ecological Sciences 2nd Edition.
    ! Cromwell Press, Trowbridge, UK, p. 248.

    implicit none

    real(8) :: tgt_time, daylength, tx, tn, tgt_tm
    real(8) :: t_dec, t_dawn, tm, tvar
    real(8), parameter :: pi = 3.14159265d0

    tm     = (tx + tn) / 2.0d0 ! average temperature of the day
    tvar   = (tx - tn) / 2.0d0 ! variation of temperature of the day
    t_dec  = tgt_time / 24.0d0 ! from (hour) to (dec time)
    t_dawn = 0.5d0 - daylength/24.0d0/2.0d0 ! The time of dawn (dec time)

    if( t_dec <= t_dawn) then
    tgt_tm = tm - tvar*sin( 2.0d0*pi * (t_dec + 1.0d0 - &
    (1.0d0 + t_dawn + 0.625d0)/2.0d0)/(2.0d0*(1.0d0+t_dawn-0.625d0)) )
    endif

    if( t_dawn < t_dec .and. t_dec < 0.625 ) then
    tgt_tm = tm - tvar*sin( 2.0d0*pi * (t_dec - &
    (t_dawn + 0.625d0)/2.0d0)/(2.0d0*(t_dawn-0.625d0)) )
    endif

    if( 0.625 <= t_dec ) then
    tgt_tm = tm - tvar*sin( 2.0d0*pi * (t_dec - &
    (1.0d0 + t_dawn + 0.625d0)/2.0d0)/(2.0d0*(1.0d0+t_dawn-0.625d0)) )
    endif

end subroutine tm_cource

end module photosynthesis
