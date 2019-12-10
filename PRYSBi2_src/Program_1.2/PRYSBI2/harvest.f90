module harvest

use spm_param
use control_param

implicit none

private
public :: s_harvest
contains

subroutine s_harvest(  &
	year,                & !in    Year
	nsol,                & !in    Number of soil layer                                     (-)
  doy,                 & !in    Day of year
	dae,                 & !inout The number of days after emergence
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

	implicit none

	integer, intent(in)    :: year
	integer, intent(in)    :: nsol
  integer, intent(in)    :: doy
  integer, intent(inout) :: dae
	real(8), intent(inout) :: cold_heat_change
	real(8), intent(inout) :: cold_strs
	integer, intent(inout) :: non_dev
	integer, intent(inout) :: crop_flag
	real(8), intent(inout) :: fgdd
	real(8), intent(inout) :: ag_bio
	real(8), intent(inout) :: gdd
	real(8), intent(inout) :: biomass
	real(8), intent(inout) :: lai
	real(8), intent(inout) :: cht
	real(8), intent(inout) :: root_depth
	real(8), intent(inout) :: froot
	real(8), intent(inout) :: root_bio_l( nsol )
	real(8), intent(inout) :: soil_resid_l( nsol )
	real(8), intent(inout) :: leaf_bio
	real(8), intent(out)   :: yield

	integer :: flag, fail_flag
	real(8) :: ag_biot
	real(8) :: hi
	real(8) :: xxx

	flag      = 0
	fail_flag = 0

	!----- Count dae -----
	dae = dae + 1

	!----- Decide harvest or not -----
	if( fgdd <  1d0 .or.  crop_flag == 0 ) then
    flag = 0
  endif
	if( fgdd >= 1d0 .and. crop_flag == 1 ) then
    flag = 1
  endif
  if( dae > 300 .and. crop_flag == 1 .and. target_plant /= "wheat_winter" ) then
    flag = 1
    fail_flag = 1
  endif
  if( dae > 300   .and. crop_flag == 1 .and. target_plant == "wheat_winter" ) then
    flag = 1
  endif

  !------ Considering non-developing days ----- insert
  if( non_dev > 10 .and. crop_flag == 1 .and. fgdd > 0.5d0 .and. target_plant /= "wheat_winter") then
    flag = 1
  endif

  if( hvst_lt_plnt == 1 .and. crop_flag == 1 .and. doy == 365 ) then
    flag = 1
    fail_flag = 1
  endif

    !----- When not matured -----
	if( flag == 0 ) then

    yield = -9999d0

	endif

	!----- When matured -----
	if( flag == 1 ) then

		!--- Conversion ---
		biomass = biomass * 0.001d0 ! From (kg/ha) to (t/ha)

		!--- Calculate HI change ---
		if( hi_change_type == 0 ) then
			hi = basehimx
		endif

		if( hi_change_type == 1 ) then
			hi = basehimx + ic_himx*dble( year - base_year )
		endif

		!--- Calculate yield considering cold stress ---
		if( cold_heat_type == 0) then
			yield = hi * yield_correct * biomass
		endif
		if( cold_heat_type == 1) then
!			yield = hi * yield_correct * biomass + 0.01d0*cold_heat_change
			yield = hi * yield_correct * biomass * cold_heat_change

		endif

		!--- Correction ---
    if( fail_flag == 1 ) then
      yield     = 0d0
    endif

		if( yield < 0d0 ) then
      yield     = 0d0
      fail_flag = 1
		endif

		!--- Residual ---
		xxx = ag_resid_rate*ag_bio
		soil_resid_l(1) = soil_resid_l(1) + xxx
		soil_resid_l(:) = soil_resid_l(:) + root_bio_l(:)

		!--- Reset ---
		crop_flag           = 0
		fgdd                = 0d0
		ag_bio              = 0d0
		leaf_bio            = 0d0
		lai                 = 0d0
		cht                 = 0d0
		gdd                 = 0d0
		root_depth          = 0d0
		root_bio_l(:)       = 0d0
		non_dev             = 0
		cold_strs           = 0d0
		biomass             = 0d0
		cold_heat_change    = 0d0
		dae                 = 0
		froot               = 0d0

	endif

end subroutine s_harvest

end module harvest
