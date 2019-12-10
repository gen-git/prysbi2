module planting

use spm_param

implicit none

private
public :: s_planting
contains

subroutine s_planting( &
	tm,         &
	plnt_dayin, & !in    Planting day                                             (day)
	doy,        & !in    Day of years                                             (day)
	fgdd,       & !in    The rate of gdd to tgdd                                  (-)
	crop_flag,  & !inout There is crop (1) or not (0)                             (-)
	lg_days,    & !inout Leaf growth days                                         (-)
	biomass,    & !inout Biomass                                                  (kg dry/ha)
	gdd)          !inout Growth degree days                                       (deg C)
	
	implicit none

	real(8), intent(in)    :: tm
	integer, intent(in)    :: plnt_dayin
	integer, intent(in)    :: doy
	real(8), intent(in)    :: fgdd
	integer, intent(inout) :: crop_flag
	integer, intent(inout) :: lg_days
	real(8), intent(inout) :: biomass
	real(8), intent(inout) :: gdd

    integer :: emerging_day
	
	!----------------------------------------------------------------------------------------------!
	!                  year 1              year 2                                                  !
	!                  ------------------------------------------------------------------------>   !
	!                                                planting                  harvest             !
	! crop_flag        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0     !
	!----------------------------------------------------------------------------------------------!

    !--- Calculate emerging day ---
    emerging_day = plnt_dayin + int(l_emerge)
    if(emerging_day > 365) then
        emerging_day = emerging_day - 365
    endif
	
    ! planting
    if( crop_flag == 0	) then
        if( doy == emerging_day ) then
            crop_flag = 1
            gdd       = 0d0
            biomass   = init_bio
            lg_days   = 0
        endif
    endif

	!--- Calculate leaf growth days ---
    if( crop_flag == 1 ) then
        lg_days = lg_days + 1
    endif

end subroutine s_planting

end module planting
