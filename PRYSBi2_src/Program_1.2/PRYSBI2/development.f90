module development

use spm_param

implicit none

private
public :: s_development
contains 

subroutine s_development( &
	tm,        & !in    Average temperature                                    (deg C)
	non_dev,   & !inout continuous non-developing days                         (days)
	gdd,       & !inout Growing degree days                                    (deg C)
	fgdd)        !out   Fraction of GDD to total GDD needed for maturity (0-1) (-)
	
	implicit none

	real(8), intent(in)    :: tm
	integer, intent(inout) :: non_dev
	real(8), intent(inout) :: gdd
	real(8), intent(out)   :: fgdd
	
	real(8) :: dgdd
	
	
	dgdd = max( 0d0, tm - tmbs )
	
	gdd  = gdd + dgdd
	
	fgdd = gdd / tgdd
	
	if( dgdd >  0d0 ) non_dev = 0
	if( dgdd == 0d0 ) non_dev = non_dev + 1
	
end subroutine s_development

end module development
