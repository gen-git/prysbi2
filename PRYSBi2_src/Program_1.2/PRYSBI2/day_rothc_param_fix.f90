module day_rothc_param_fix

use day_rothc_param
use control_param

implicit none

private
public :: s_day_rothc_param_fix

contains

subroutine s_day_rothc_param_fix

	implicit none
	
	! Fixing parameter
	Wparam( 1:4 ) = 1.0d0    ! Parameter adjusting water effect (basically Wparam = 1.0)
	Lparam( 1:4 ) = 1.0d0    ! Parameter adjusting basic decomposition rate (basically Lparam = 1.0)
	
 	Rparam( 1 ) = 97.62d0    ! Value calcuated in Japan soil (Sakurai et al. 2012 SBB) (in RothC Rparam = 106.0)
 	Rparam( 2 ) = 149.53d0   ! Value calcuated in Japan soil (Sakurai et al. 2012 SBB) (in RothC Rparam = 106.0)
 	Rparam( 3 ) = 141.60d0   ! Value calcuated in Japan soil (Sakurai et al. 2012 SBB) (in RothC Rparam = 106.0)
 	Rparam( 4 ) = 134.18d0   ! Value calcuated in Japan soil (Sakurai et al. 2012 SBB) (in RothC Rparam = 106.0)
 	
 	! Read namelist
 	if( read_spm_param_namelist == 1 ) then
	 	open(1, file=prysbi_namelist_file)
	 	read(1, day_rothc_namelist)
	 	close(1)
 	endif
end subroutine s_day_rothc_param_fix

end module day_rothc_param_fix
