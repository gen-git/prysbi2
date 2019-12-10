module swatnut_param_fix

use swatnut_param
use control_param

implicit none

private
public :: s_swatnut_param_fix
contains

subroutine s_swatnut_param_fix

implicit none

real(8) :: bb1, bb2, bb3, bp1, bp2, bp3


if( target_plant == 'soybean' ) then

	!----- Parameters -----
	rsdco_pl   = 0.05d0   ! Plant residue decomposition coefficient                             (SWAT default) C3, C4
	sdnco      = 1.1d0    ! Denitrification threshold: fraction of field                        (SWAT default) C3, C4
	cmn        = 0.0003d0 ! Rate factor for humus mineralization                                (SWAT default) C3, C4
	nactfr     = 0.02d0   ! Nitrogen active pool fraction                                       (SWAT default) C3, C4
	cdn        = 1.4d0    ! Coefficient of denitrification                                      (SWAT default) C3, C4
	cecf       = 0.15d0   ! Volatilization CEC factor [param]                                   (SWAT default) C3, C4
	anion_excl = 0.5d0    ! Fraction of porosity from which anions are excluded                 (SWAT default) C3, C4
	nperco     = 0.2d0    ! Nitrate percolation coefficient                                     (SWAT default) C3, C4
	psp        = 0.4d0    ! Phosphorus availability index                                       (SWAT default) C3, C4
	bk         = 0.0006d0 ! Phosphorus stabilization coef                                       (SWAT default) C3, C4
	phoskd     = 175.0d0  ! Phosphorus soil partitioning coefficient                            (SWAT default) C3, C4
	pperco     = 10.0d0   ! Phosphorus percolation coefficient (0-1)                            (SWAT default) C3, C4
	rcn_sub    = 0.2d0    ! Concentration of nitrogen in the rainfall (mg/L)                    (SWAT default) C3, C4
	n_updis    = 20.0d0   ! Nitrogen uptake distribution parameter                              (SWAT default) C3, C4
	p_updis    = 20.0d0   ! Phosphorus uptake distribution parameter                            (SWAT default) C3, C4
	nfrth      = 0.5d0    ! Threshold of N stress                                               (SWAT default) C3, C4
	pfrth      = 0.5d0    ! Threshold of P stress                                               (SWAT default) C3, C4
	cons_nfix  = 1        ! Consider nitrogen fixation (1) or not (0)                           (SWAT default) Legume

	!----- Provisional values -----
	pltnfr1    = 0.0524d0 ! Biomass|nitrogen uptake parameter 1 ( change with crop (bn1) )      (SWAT default) Soybeam
	pltnfr2    = 0.0265d0 ! Biomass|nitrogen uptake parameter 2 ( change with crop (bn2) )      (SWAT default) Soybean
	pltnfr3    = 0.0258d0 ! Biomass|nitrogen uptake parameter 3 ( change with crop (bn3) )      (SWAT default) Soybean
	pltnfrav   = 0.0650d0 ! Normal fraction of nitrogen in the plant biom. near mat. (cnyldc)   (SWAT default) Soybean
	pltpfr1    = 0.0074d0 ! Biomass|phosphorus uptake parameter 1 ( change with crop (bp1) )    (SWAT default) Soybean
	pltpfr2    = 0.0037d0 ! Biomass|phosphorus uptake parameter 2 ( change with crop (bp2) )    (SWAT default) Soybean
	pltpfr3    = 0.0035d0 ! Biomass|phosphorus uptake parameter 3 ( change with crop (bp3) )    (SWAT default) Soybean
	pltpfrav   = 0.0091d0 ! Normal fraction of phosphorus in the plant biom. near mat. (cnyldc) (SWAT default) Soybean
	

endif


if( target_plant == 'wheat_winter' ) then

	!----- Parameters -----
	rsdco_pl   = 0.05d0   ! Plant residue decomposition coefficient                             (SWAT default) C3, C4
	sdnco      = 1.1d0    ! Denitrification threshold: fraction of field                        (SWAT default) C3, C4
	cmn        = 0.0003d0 ! Rate factor for humus mineralization                                (SWAT default) C3, C4
	nactfr     = 0.02d0   ! Nitrogen active pool fraction                                       (SWAT default) C3, C4
	cdn        = 1.4d0    ! Coefficient of denitrification                                      (SWAT default) C3, C4
	cecf       = 0.15d0   ! Volatilization CEC factor [param]                                   (SWAT default) C3, C4
	anion_excl = 0.5d0    ! Fraction of porosity from which anions are excluded                 (SWAT default) C3, C4
	nperco     = 0.2d0    ! Nitrate percolation coefficient                                     (SWAT default) C3, C4
	psp        = 0.4d0    ! Phosphorus availability index                                       (SWAT default) C3, C4
	bk         = 0.0006d0 ! Phosphorus stabilization coef                                       (SWAT default) C3, C4
	phoskd     = 175.0d0  ! Phosphorus soil partitioning coefficient                            (SWAT default) C3, C4
	pperco     = 10.0d0   ! Phosphorus percolation coefficient (0-1)                            (SWAT default) C3, C4
	rcn_sub    = 0.2d0    ! Concentration of nitrogen in the rainfall (mg/L)                    (SWAT default) C3, C4
	n_updis    = 20.0d0   ! Nitrogen uptake distribution parameter                              (SWAT default) C3, C4
	p_updis    = 20.0d0   ! Phosphorus uptake distribution parameter                            (SWAT default) C3, C4
	nfrth      = 0.5d0    ! Threshold of N stress                                               (SWAT default) C3, C4
	pfrth      = 0.5d0    ! Threshold of P stress                                               (SWAT default) C3, C4
	cons_nfix  = 0        ! Consider nitrogen fixation (1) or not (0)                           (SWAT default) Non legume

	!----- Provisional values -----
	pltnfr1    = 0.0663d0 ! Biomass|nitrogen uptake parameter 1 ( change with crop (bn1) )      (SWAT default) Winter wheat
	pltnfr2    = 0.0255d0 ! Biomass|nitrogen uptake parameter 2 ( change with crop (bn2) )      (SWAT default) Winter wheat
	pltnfr3    = 0.0148d0 ! Biomass|nitrogen uptake parameter 3 ( change with crop (bn3) )      (SWAT default) Winter wheat
	pltnfrav   = 0.0250d0 ! Normal fraction of nitrogen in the plant biom. near mat. (cnyldc)   (SWAT default) Winter wheat
	pltpfr1    = 0.0053d0 ! Biomass|phosphorus uptake parameter 1 ( change with crop (bp1) )    (SWAT default) Winter wheat
	pltpfr2    = 0.0020d0 ! Biomass|phosphorus uptake parameter 2 ( change with crop (bp2) )    (SWAT default) Winter wheat
	pltpfr3    = 0.0012d0 ! Biomass|phosphorus uptake parameter 3 ( change with crop (bp3) )    (SWAT default) Winter wheat
	pltpfrav   = 0.0022d0 ! Normal fraction of phosphorus in the plant biom. near mat. (cnyldc) (SWAT default) Winter wheat

endif


if(target_plant == 'maize' .or. target_plant == 'maize_major' .or. target_plant == 'maize_second' ) then

	!----- Parameters -----
	rsdco_pl   = 0.05d0   ! Plant residue decomposition coefficient                             (SWAT default) C3, C4
	sdnco      = 1.1d0    ! Denitrification threshold: fraction of field                        (SWAT default) C3, C4
	cmn        = 0.0003d0 ! Rate factor for humus mineralization                                (SWAT default) C3, C4
	nactfr     = 0.02d0   ! Nitrogen active pool fraction                                       (SWAT default) C3, C4
	cdn        = 1.4d0    ! Coefficient of denitrification                                      (SWAT default) C3, C4
	cecf       = 0.15d0   ! Volatilization CEC factor [param]                                   (SWAT default) C3, C4
	anion_excl = 0.5d0    ! Fraction of porosity from which anions are excluded                 (SWAT default) C3, C4
	nperco     = 0.2d0    ! Nitrate percolation coefficient                                     (SWAT default) C3, C4
	psp        = 0.4d0    ! Phosphorus availability index                                       (SWAT default) C3, C4
	bk         = 0.0006d0 ! Phosphorus stabilization coef                                       (SWAT default) C3, C4
	phoskd     = 175.0d0  ! Phosphorus soil partitioning coefficient                            (SWAT default) C3, C4
	pperco     = 10.0d0   ! Phosphorus percolation coefficient (0-1)                            (SWAT default) C3, C4
	rcn_sub    = 0.2d0    ! Concentration of nitrogen in the rainfall (mg/L)                    (SWAT default) C3, C4
	n_updis    = 20.0d0   ! Nitrogen uptake distribution parameter                              (SWAT default) C3, C4
	p_updis    = 20.0d0   ! Phosphorus uptake distribution parameter                            (SWAT default) C3, C4
	nfrth      = 0.5d0    ! Threshold of N stress                                               (SWAT default) C3, C4
	pfrth      = 0.5d0    ! Threshold of P stress                                               (SWAT default) C3, C4
	cons_nfix  = 0        ! Consider nitrogen fixation (1) or not (0)                           (SWAT default) Non legume

	!----- Provisional values -----
	pltnfr1    = 0.0470d0 ! Biomass|nitrogen uptake parameter 1 ( change with crop (bn1) )      (SWAT default) Maize
	pltnfr2    = 0.0177d0 ! Biomass|nitrogen uptake parameter 2 ( change with crop (bn2) )      (SWAT default) Maize
	pltnfr3    = 0.0138d0 ! Biomass|nitrogen uptake parameter 3 ( change with crop (bn3) )      (SWAT default) Maize
	pltnfrav   = 0.0140d0 ! Normal fraction of nitrogen in the plant biom. near mat. (cnyldc)   (SWAT default) Maize
	pltpfr1    = 0.0048d0 ! Biomass|phosphorus uptake parameter 1 ( change with crop (bp1) )    (SWAT default) Maize
	pltpfr2    = 0.0018d0 ! Biomass|phosphorus uptake parameter 2 ( change with crop (bp2) )    (SWAT default) Maize
	pltpfr3    = 0.0014d0 ! Biomass|phosphorus uptake parameter 3 ( change with crop (bp3) )    (SWAT default) Maize
	pltpfrav   = 0.0016d0 ! Normal fraction of phosphorus in the plant biom. near mat. (cnyldc) (SWAT default) Maize

endif


if( target_plant == 'rice' .or. target_plant == 'rice_major' .or. target_plant == 'rice_second' ) then

	!----- Parameters -----
	rsdco_pl   = 0.05d0   ! Plant residue decomposition coefficient                             (SWAT default) C3, C4
	sdnco      = 1.1d0    ! Denitrification threshold: fraction of field                        (SWAT default) C3, C4
	cmn        = 0.0003d0 ! Rate factor for humus mineralization                                (SWAT default) C3, C4
	nactfr     = 0.02d0   ! Nitrogen active pool fraction                                       (SWAT default) C3, C4
	cdn        = 1.4d0    ! Coefficient of denitrification                                      (SWAT default) C3, C4
	cecf       = 0.15d0   ! Volatilization CEC factor [param]                                   (SWAT default) C3, C4
	anion_excl = 0.5d0    ! Fraction of porosity from which anions are excluded                 (SWAT default) C3, C4
	nperco     = 0.2d0    ! Nitrate percolation coefficient                                     (SWAT default) C3, C4
	psp        = 0.4d0    ! Phosphorus availability index                                       (SWAT default) C3, C4
	bk         = 0.0006d0 ! Phosphorus stabilization coef                                       (SWAT default) C3, C4
	phoskd     = 175.0d0  ! Phosphorus soil partitioning coefficient                            (SWAT default) C3, C4
	pperco     = 10.0d0   ! Phosphorus percolation coefficient (0-1)                            (SWAT default) C3, C4
	rcn_sub    = 0.2d0    ! Concentration of nitrogen in the rainfall (mg/L)                    (SWAT default) C3, C4
	n_updis    = 20.0d0   ! Nitrogen uptake distribution parameter                              (SWAT default) C3, C4
	p_updis    = 20.0d0   ! Phosphorus uptake distribution parameter                            (SWAT default) C3, C4
	nfrth      = 0.5d0    ! Threshold of N stress                                               (SWAT default) C3, C4
	pfrth      = 0.5d0    ! Threshold of P stress                                               (SWAT default) C3, C4
	cons_nfix  = 0        ! Consider nitrogen fixation (1) or not (0)                           (SWAT default) Non legume

	!----- Provisional values -----
	pltnfr1    = 0.0500d0 ! Biomass|nitrogen uptake parameter 1 ( change with crop (bn1) )      (SWAT default) rice
	pltnfr2    = 0.0200d0 ! Biomass|nitrogen uptake parameter 2 ( change with crop (bn2) )      (SWAT default) rice
	pltnfr3    = 0.0100d0 ! Biomass|nitrogen uptake parameter 3 ( change with crop (bn3) )      (SWAT default) rice
	pltnfrav   = 0.0136d0 ! Normal fraction of nitrogen in the plant biom. near mat. (cnyldc)   (SWAT default) rice
	pltpfr1    = 0.0060d0 ! Biomass|phosphorus uptake parameter 1 ( change with crop (bp1) )    (SWAT default) rice
	pltpfr2    = 0.0030d0 ! Biomass|phosphorus uptake parameter 2 ( change with crop (bp2) )    (SWAT default) rice
	pltpfr3    = 0.0018d0 ! Biomass|phosphorus uptake parameter 3 ( change with crop (bp3) )    (SWAT default) rice
	pltpfrav   = 0.0013d0 ! Normal fraction of phosphorus in the plant biom. near mat. (cnyldc) (SWAT default) rice

endif


if( target_plant == 'wheat_spring' ) then

	!----- Parameters -----
	rsdco_pl   = 0.05d0   ! Plant residue decomposition coefficient                             (SWAT default) C3, C4
	sdnco      = 1.1d0    ! Denitrification threshold: fraction of field                        (SWAT default) C3, C4
	cmn        = 0.0003d0 ! Rate factor for humus mineralization                                (SWAT default) C3, C4
	nactfr     = 0.02d0   ! Nitrogen active pool fraction                                       (SWAT default) C3, C4
	cdn        = 1.4d0    ! Coefficient of denitrification                                      (SWAT default) C3, C4
	cecf       = 0.15d0   ! Volatilization CEC factor [param]                                   (SWAT default) C3, C4
	anion_excl = 0.5d0    ! Fraction of porosity from which anions are excluded                 (SWAT default) C3, C4
	nperco     = 0.2d0    ! Nitrate percolation coefficient                                     (SWAT default) C3, C4
	psp        = 0.4d0    ! Phosphorus availability index                                       (SWAT default) C3, C4
	bk         = 0.0006d0 ! Phosphorus stabilization coef                                       (SWAT default) C3, C4
	phoskd     = 175.0d0  ! Phosphorus soil partitioning coefficient                            (SWAT default) C3, C4
	pperco     = 10.0d0   ! Phosphorus percolation coefficient (0-1)                            (SWAT default) C3, C4
	rcn_sub    = 0.2d0    ! Concentration of nitrogen in the rainfall (mg/L)                    (SWAT default) C3, C4
	n_updis    = 20.0d0   ! Nitrogen uptake distribution parameter                              (SWAT default) C3, C4
	p_updis    = 20.0d0   ! Phosphorus uptake distribution parameter                            (SWAT default) C3, C4
	nfrth      = 0.5d0    ! Threshold of N stress                                               (SWAT default) C3, C4
	pfrth      = 0.5d0    ! Threshold of P stress                                               (SWAT default) C3, C4
	cons_nfix  = 0        ! Consider nitrogen fixation (1) or not (0)                           (SWAT default) Non legume

	!----- Provisional values -----
	pltnfr1    = 0.0600d0 ! Biomass|nitrogen uptake parameter 1 ( change with crop (bn1) )      (SWAT default) Spring wheat
	pltnfr2    = 0.0231d0 ! Biomass|nitrogen uptake parameter 2 ( change with crop (bn2) )      (SWAT default) Spring wheat
	pltnfr3    = 0.0134d0 ! Biomass|nitrogen uptake parameter 3 ( change with crop (bn3) )      (SWAT default) Spring wheat
	pltnfrav   = 0.0234d0 ! Normal fraction of nitrogen in the plant biom. near mat. (cnyldc)   (SWAT default) Spring wheat
	pltpfr1    = 0.0084d0 ! Biomass|phosphorus uptake parameter 1 ( change with crop (bp1) )    (SWAT default) Spring wheat
	pltpfr2    = 0.0032d0 ! Biomass|phosphorus uptake parameter 2 ( change with crop (bp2) )    (SWAT default) Spring wheat
	pltpfr3    = 0.0019d0 ! Biomass|phosphorus uptake parameter 3 ( change with crop (bp3) )    (SWAT default) Spring wheat
	pltpfrav   = 0.0033d0 ! Normal fraction of phosphorus in the plant biom. near mat. (cnyldc) (SWAT default) Spring wheat

endif


!----- Calculate parameter -----
bb1 = pltnfr1 - pltnfr3
bb2 = 1.0d0 - ( pltnfr2 - pltnfr3 ) / bb1
bb3 = 1.0d0 - 0.00001d0 / bb1

bp1 = pltpfr1 - pltpfr3
bp2 = 1.0d0 - ( pltpfr2 - pltpfr3 ) / bp1
bp3 = 1.0d0 - 0.00001d0 / bp1

call Mascrv( bb2, bb3, 0.5d0, 1.0d0, bio_n1, bio_n2 )
call Mascrv( bp2, bp3, 0.5d0, 1.0d0, bio_p1, bio_p2 )


end subroutine s_swatnut_param_fix
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

end module swatnut_param_fix
